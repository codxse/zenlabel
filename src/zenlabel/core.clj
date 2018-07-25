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
  #{"Bab" "bab" "-" "01" "02" "03" "04" "05" "06" "07" "08" "09" "1" "2" "3" "4" "5" "6" "7" "8" "9"})

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
    (for [[subject chapters] (sort distincted-chapters)]
      {:subject subject
       :enum-name (create-enum "cbt-exam.chapter" subject)
       :chapters (into []
                   (for [[chapter meta-data] (sort chapters)]
                     {:chapter-name chapter
                      :enum-name (create-enum "teacher-cbt-problem" chapter)
                      :contents meta-data}))})))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->> (group-by-subject DATA)
       distinct-chapters
       into-better-data-structure
       pprint
       with-out-str
       (spit "subjects.edn")))
