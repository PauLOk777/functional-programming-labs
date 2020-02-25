(ns lab3.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn getFile [name]
  (def string1 (slurp name))
  string1)

(defn makeTableCSV [name]
  (map #(clojure.string/split % #",")
       (clojure.string/split (getFile name) #"\n")))

(defn printTableCSV [table]
  (loop [i 0]
    (when (< i (count table))
      (loop [j 0]
        (when (< j (count (nth table i)))
          (print (format "%35s|" (nth (nth table i) j)))
          (recur (+ j 1))))
      (println "\n")
      (recur (+ i 1)))))

(defn checkFormat [name]
  (def formatFile (clojure.string/split name #"\."))
  (if (= (str (nth formatFile 1)) "csv") true false))

(defn -main
  [& args]
  (println "Input name of file: ")
  (def input (read-line))
  ;(def input "mp-posts_full.csv")
  (if (checkFormat input)
    (printTableCSV (makeTableCSV input))
    (println "Hello")))

;(getRegexp 3)
;
;(loop [x 0]
;  (when (< x 5)
;    (println (format "%s|%s" "daun" "eblan"))
;    (recur (+ x 1))))
;(format "%s" (nth [1 "dd"] 1))