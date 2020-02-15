(ns lab2.core
  (:gen-class))

; Реалізувати функцію для генерації послідовності
; із заданою кількістю елементів на виході
; 1/2 − 1/4 + 1/8 − 1/16...
(def valueMain 2)
(defn evlt [value degree]
  (reduce * (for [i (range degree)]
              value)))

(defn mySeq [number]
  (map #(/ 1 %) (for [i (range 1 (+ 1 number))]
                  (if (odd? i)
                    (evlt valueMain i)
                    (- (evlt valueMain i))))))

; Реалізувати сортування послідовності
; із першого завдання merge sort
(defn mergestep [l r ord option]
  (cond (empty? l) r
        (empty? r) l
        :else
          (if (ord (compare (option (first l)) (option (first r))))
            (cons (first l) (mergestep (next l) r ord option))
            (cons (first r) (mergestep l (next r) ord option)))))


(defn mergesort
  ([data] (mergesort data neg?))
  ([data ord] (mergesort data ord max))
  ([data ord option]
   (if (< (count data) 2)
     data
     (mergestep
       (mergesort (first (split-at (/ (count data) 2) data)) ord option)
       (mergesort (second (split-at (/ (count data) 2) data)) ord option) ord option))))

(mergesort (mySeq 8) pos?)
(mergesort '("a" "d" "b") neg?)

; 1. Реалізувати сортування за алфавітом послідовності
; слів використовуючи алгоритм з попереднього завдання
; 2. Реалізувати сортування за довжиною слова послідовності
; слів використовуючи алгоритм з попереднього завдання
(def text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
(def lowerCaseText (clojure.string/lower-case text))
(def textNoDot (clojure.string/replace lowerCaseText #"[.,]" ""))
(def clearText (clojure.string/split textNoDot #" "))
; Сортування за алфавітом
(mergesort clearText neg?)
; Сортування за довжиною рядка
(mergesort clearText neg? count)