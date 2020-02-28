(ns lab4-7.core
  (:gen-class)
  (:use [clojure.data.csv])
  (:require [clojure.string :as str]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn readTSV [name]
  (map #(str/split % #"\t")
       (str/split (slurp name) #"\r\n")))

(defn readCSV [name]
  (with-open [reader (io/reader name)]
    (doall
      (csv/read-csv reader))))

(defn data->maps [head & lines]
  (map #(zipmap (map keyword head) %1) lines))

(defn makeTableTSV [name]
  (apply data->maps (readTSV name)))

(defn makeTableCSV [name]
  (apply data->maps (readCSV name)))

(defn printTable [table]
  (loop [k 0]
    (when (< k (count (first table)))
      (print (format "%40s| " (name (nth (keys (first table)) k))))
      (recur (+ k 1))))
  (println)
  (loop [i 0]
    (when (< i (count table))
      (loop [j 0]
        (when (< j (count (nth table i)))
          (print (format "%40s| " (nth (vals (nth table i)) j)))
          (recur (+ j 1))))
      (println "")
      (recur (+ i 1)))))

(defn checkFormat [name]
  (def formatFile (str/split name #"\."))
  (if (= (str (nth formatFile 1)) "csv") true false))

(defn getFile [name]
  (if (checkFormat name)
    (makeTableCSV name)
    (makeTableTSV name)))

(defn getMainTable [columns fileName]
  (def file (getFile fileName))
  file)

(defn withDistinct [expr commands]
  [])

(defn noDistinct [words commands]
  (cond
    (not= 0 (compare (nth words 2) (nth commands 1))) []
    :else
    (getMainTable (str/split (nth words 1) #",") (nth words 3))))

(defn processColumns [words commands]
  (if (not= 0 (compare (nth words 1) (nth commands 3)))
    (noDistinct words commands)
    (withDistinct words commands)))

(defn getResult [expr commands]
  (def words (str/split expr #"\s"))
  (if (not= 0 (compare (first words) (first commands))) []
    (processColumns words commands)))

(defn -main
  [& args]
  (println "Write your query below this: ")
  (def input (read-line))
  (def commands ["SELECT" "FROM" "WHERE" "DISTINCT"])
  (getResult input commands))

(-main)