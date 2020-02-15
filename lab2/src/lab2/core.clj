(ns lab2.core
  (:gen-class))

; Реалізувати функцію для генерації послідовності
; із заданою кількістю елементів на виході
; 1/2 − 1/4 + 1/8 − 1/16...
(def valueMain 2)
(defn evlt [value degree]
  (reduce * (for [i (range degree)]
              value)))

(defn checkForOdd [number]
  (if (odd? number) true false))

(defn mySeq [number]
  (map #(/ 1 %) (for [i (range 1 (+ 1 number))]
                  (if (checkForOdd i)
                    (evlt valueMain i)
                    (- (evlt valueMain i))))))

(mySeq 8)