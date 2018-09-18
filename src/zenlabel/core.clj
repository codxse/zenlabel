(ns zenlabel.core
  (:require [dk.ative.docjure.spreadsheet :as xls]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(def ^{:private true} SUBJECTS
  #"([M|m]atematika [W|w]ajib|[m|M]atematika [p:P]eminatan|[m|M]atematika [S|s][M|m][p|P]|[M|m]atematika||[b|B]ahasa [I|i]ndonesia|Bahasa Inggris|IPA|Biologi|Fisika|Kimia|Sejarah Indonesia|Sejarah Peminatan|Ekonomi|Geografi|Sosiologi)")

(def ^{:private true} SUBJECTS
  #"((?i)matematika ipa|(?i)matematika-ipa|(?i)matematika ips|(?i)matematika-ips|(?i)matematika dasar|(?i)matematika-dasar|(?i)matematika wajib|(?i)matematika-wajib|(?i)matematika peminatan|(?i)matematika-peminatan|(?i)matematika|(?i)bahasa indonesia|(?i)bahasa-indonesia|(?i)bahasa inggris|(?i)bahasa-inggris|(?i)biologi|(?i)fisika|(?i)kimia|(?i)sejarah indonesia|(?i)sejarah-indonesia|(?i)sejarah peminatan|(?i)sejarah-peminatan|(?i)sejarah|(?i)ekonomi|(?i)geografi|(?i)sosiologi|(?i)tpa|(?i)ipa|(?i)ips)")

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

(defn make-chapter [m]
  (->> (s/split (:cg-parent m) #"\s")
       (remove NEGATIVE_WORDS)
       (interpose " ")
       s/join))

(defn- distinct-chapters
  [grouped-subjects]
  (into (sorted-map)
    (for [[subject meta-data] (sort grouped-subjects)]
      (let [chapter make-chapter]
        [subject (group-by chapter meta-data)]))))

(defn- create-enum [namespace-name enum]
  (->> (s/replace enum #"(\s|\(|\)|\,|\.|\?|\{|\}|\[|\]|\\|\/|\<|\>|\:|\;|\+|\=|\*|\^|\&|\%|\$|\#|\@|\!|\`|\~)" "-")
       ;(str namespace-name "/")
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

;(for [{:keys [subject original-name chapters]}
;      (->> (group-by-subject DATA) distinct-chapters into-better-data-structure)]
;  {:upper [subject original-name]
;   :chapters (map #(select-keys % [:kode-contents :chapter-name])
;                  chapters)})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->> (group-by-subject DATA)
       distinct-chapters
       into-better-data-structure
       (map (fn [m]
              {:subject (:enum-name m)
               ;:label (:subject m)
               :chapters (mapv :enum-name (:chapters m))}))
       pprint
       with-out-str
       (spit "subjects.edn")))

(def mbek (let [cgs (group-by :kode-cg-parent DATA)]
            (->> (for [[k v] (sort cgs)]
                   [(int k) v])
                 (into {}))))

(defn remove-not-mbek
  [m]
  (let [key-set (set (keys mbek))]
    (contains? key-set (:cg-id m))))

;(def mbek2 (->> (for [[k val] (sort mbek)]
;                  [k (->> val
;                         (mapv (fn [m]
;                                (let [chapter (make-chapter m)]
;                                  (-> (assoc m :enum-name (create-enum nil chapter))
;                                      (dissoc :kode-cg-pelajaran-tingkat-kurikulum
;                                              :pelajaran-tingkat-kurikulum
;                                              :kode-content
;                                              :kode-cg-parent
;                                              :total-video
;                                              :judul-content
;                                              :durasi-detik))))
;                          (distinct)
;                          first))])
;                (into {})))

;; =======================================================
;; New request chapters by mbek

;; to get upper level sd, smp, sma, sbmptn, dll
(def slugged-cg (clojure.edn/read-string (slurp "slugged-cg.edn")))
;; (def clean-slugged-cg (remove remove-not-mbek slugged-cg))

;; to get the chapters
(def content (clojure.edn/read-string (slurp "content.edn")))
(def clean-content (remove remove-not-mbek content))

(defn make-subject-from-slugged-2-vec
  [slugged-position-2]
  (->> (s/split slugged-position-2 #"-")
       (remove NEGATIVE_WORDS)
       (interpose " ")
       s/join))

(def pair-content-slugged-cg
  (let []
    (->> (for [scg slugged-cg
               cc content
               :when (= (:cg-id scg) (:cg-id cc))]
           (assoc cc :tags-raw (:tags-raw scg)))
         (map (fn [m]
                (assoc m :tags-raw-vec (clojure.string/split (:tags-raw m) #",")
                         :name-raw (str (:tags-raw m) " " (:name m) " " (:browser-window-title m)))))
         (map (fn [m]
                (assoc m :subject-str (-> (re-matcher SUBJECTS (:name-raw m))
                                          re-find
                                          first))))
         (remove (fn [m]
                   (nil? (:subject-str m))))
         (map (fn [m]
                (let [subject-str (:subject-str m)
                      subject-str-lowercase (clojure.string/lower-case subject-str)
                      subject-keyword (->> (s/split subject-str-lowercase #"\s")
                                           (interpose "-")
                                           (apply str)
                                           keyword)]
                  (assoc m :subject-keyword (if (= :tpa subject-keyword) :TPA subject-keyword)
                           :chapter (->> (clojure.string/split (:canonical-name m) #"-")
                                         (remove NEGATIVE_WORDS)
                                         (interpose "-")
                                         clojure.string/join
                                         keyword))))))))

(def pair-content-slugged-cg-by-subject
  (group-by :subject-keyword pair-content-slugged-cg))

(def pair-content-slugged-cg-by-cg
  (let [gc (group-by :cg-id pair-content-slugged-cg)]
    (->> (for [[k values] (sort gc)]
           [k (mapv #(select-keys % [:subject-keyword :chapter :canonical-name]) values)])
         (into (sorted-map)))))

(defn get-subject-chapter-map
  []
  (->> (for [[subject values] (sort pair-content-slugged-cg-by-subject)]
          [subject (->> (distinct (map :chapter values))
                        (remove #(= subject (:chapter %)))
                        sort vec)])
       (into {})))