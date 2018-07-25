(ns zenlabel.core
  (:require [dk.ative.docjure.spreadsheet :as xls]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(def ^{:private true} SUBJECTS
  #"(Matematika Wajib|Matematika Peminatan|Matematika SMP|Bahasa Indonesia|Bahasa Inggris|IPA|Biologi|Fisika|Kimia|Sejarah Indonesia|Sejarah Peminatan|Ekonomi|Geografi|Sosiologi)")

(def ^{:private true} DATA
  (->> (xls/load-workbook "k13.xlsx")
       (xls/select-sheet "Default")
       (xls/select-columns
         {:A :kode-cg-pelajaran-tingkat-kurikulum
          :B :pelajaran-tingkat-kurikulum
          :C :kode-content
          :D :judul-content
          :E :kode-cg-parent
          :F :cg-parent
          :G :total-video
          :H :durasi-detik})
       rest))

(def ^{:private true} NEGATIVE_WORDS
  (apply merge
    (->> (range 1 50) (map str) set)
    #{"Bab" "bab" "-" "01" "02" "03" "04" "05" "06" "07" "08" "09"}))

(defn group-by-subject
  [data]
  (group-by
    (fn [row]
      (-> (re-matcher SUBJECTS (:pelajaran-tingkat-kurikulum row))
          re-find
          first))
    data))

(defn- distinct-chapters
  [grouped-subjects]
  (into (sorted-map)
    (for [[subject meta-data] (sort grouped-subjects)]
      (let [chapter (fn [m]
                      (->> (s/split (:cg-parent m) #"\s")
                         (remove NEGATIVE_WORDS)
                         (interpose " ")
                         s/join))]
        [subject (group-by chapter meta-data)]))))

(defn- create-enum [namespace-name enum]
  (->> (s/replace enum #"(\s|\(|\)|\,|\.|\?|\{|\}|\[|\]|\\|\/|\<|\>|\:|\;|\+|\=|\*|\^|\&|\%|\$|\#|\@|\!|\`|\~)" "-")
       (str namespace-name "/")
       s/lower-case
       keyword))

(defn- into-better-data-structure
  [distincted-chapters]
  (into []
    (for [[subject chapters] (into [] distincted-chapters)]
      (let [chapters-vec (into [] chapters)]
        {:subject subject
         :original-name (->> (-> chapters-vec vals vec)
                             (map (fn [ch] (get-in ch [0 :pelajaran-tingkat-kurikulum])))
                             (into #{}))
         :enum-name (create-enum "cbt-exam.subject" subject)
         :chapters (into []
                     (for [[chapter meta-data] chapters-vec]
                       {:chapter-name chapter
                        :original-name (get-in meta-data [0 :cg-parent])
                        :kode-contents (->> meta-data
                                            (map #(do (int (:kode-content %))))
                                            (into #{}))
                        :enum-name (create-enum "teacher-cbt-problem.chapter" chapter)
                        :contents meta-data}))}))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->> (group-by-subject DATA)
       distinct-chapters
       into-better-data-structure
       pprint
       with-out-str
       (spit "subjects.edn")))
