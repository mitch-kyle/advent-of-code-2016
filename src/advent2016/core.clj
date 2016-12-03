(ns advent2016.core
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

;; Helpers 
(defn abs [x] (max x (- x)))

;; Day one
(defn dist-origin
  [x y]
  (+ (abs x)
     (abs y)))

(def direction-mapping
  {:north {"L" :west
           "R" :east}
   :west {"L" :south
          "R" :north}
   :south {"L" :east
           "R" :west}
   :east {"L" :north
          "R" :south}})

(def dir-fn-map
  {:north (fn [[x y] d] [x (+ y d)])
   :west (fn [[x y] d] [(- x d) y])
   :south (fn [[x y] d] [x (- y d)])
   :east (fn [[x y] d] [(+ x d) y])})

(defn hist
  [dir p d]
  (let [dir-fn (dir-fn-map dir)]
    (->> d
         range
         (map (partial dir-fn p))
         set)))

(defn one
  [in]
  (let [[_ [x y]] (reduce (fn [[cur-dir point] cmd]
                        (let [mv (subs cmd 0 1)
                              dist (read-string (subs cmd 1))
                              dir (get-in direction-mapping [cur-dir mv])
                              p ((dir-fn-map dir) point dist)]
                          [dir p]))
                      [:north [0 0]] (string/split in #", "))]
        (dist-origin x y)))

(defn one-two
  [in]
  (let [[found? [x y]] (reduce (fn [[cur-dir point h] cmd]
                             (let [mv (subs cmd 0 1)
                                   dist (read-string (subs cmd 1))
                                   dir (get-in direction-mapping [cur-dir mv])
                                   p ((dir-fn-map dir) point dist)
                                   v (hist dir point dist)]
                               (if-let [ans (some v h)]
                                 (reduced [true ans nil])
                                 [dir p (concat v h)])))
                           [:north [0 0] []] (string/split in #", "))]
    (if (true? found?)
      (dist-origin x y)
      false)))

;; Day Two

(def button-map
  {1 {\U 1 \D 4 \L 1 \R 2}
   2 {\U 2 \D 5 \L 1 \R 3}
   3 {\U 3 \D 6 \L 2 \R 3}
   4 {\U 1 \D 7 \L 4 \R 5}
   5 {\U 2 \D 8 \L 4 \R 6}
   6 {\U 3 \D 9 \L 5 \R 6}
   7 {\U 4 \D 7 \L 7 \R 8}
   8 {\U 5 \D 8 \L 7 \R 9}
   9 {\U 6 \D 9 \L 8 \R 9}})

;    1
;  2 3 4
;5 6 7 8 9
;  A B C
;    D
(def button-map-2
  {\1 {\U \1 \D \3 \L \1 \R \1}
   \2 {\U \2 \D \6 \L \2 \R \3}
   \3 {\U \1 \D \7 \L \2 \R \4}
   \4 {\U \4 \D \8 \L \3 \R \4}
   \5 {\U \5 \D \5 \L \5 \R \6}
   \6 {\U \2 \D \A \L \5 \R \7}
   \7 {\U \3 \D \B \L \6 \R \8}
   \8 {\U \4 \D \C \L \7 \R \9}
   \9 {\U \9 \D \9 \L \8 \R \9}
   \A {\U \6 \D \A \L \A \R \B}
   \B {\U \7 \D \D \L \A \R \C}
   \C {\U \8 \D \C \L \B \R \C}
   \D {\U \B \D \D \L \D \R \D}})

(defn two
  [btn-map in]
  (reduce (fn [[cur code] ins]
            (let [btn (reduce (fn [b i]
                                (get-in btn-map [b i]))
                              cur ins)]
              [btn (str code btn)]))
          [\5 ""] (string/split in #"\n")))

;; Day Three

(defn three
  [in]
  (reduce (fn [cnt tri]
            (let [[c b a] (->> (str "[" tri "]")
                               read-string
                               (sort >))]
              (if (< c (+ a b))
                (inc cnt)
                cnt)))
          0 (string/split in #"\n")))

(defn three-two
  [in]
  (let [in (->> (string/split in #"\n")
                (partition 3)
                (map (fn [t]
                       (reduce (fn [[x y z] s]
                                 (let [[xi yi zi] (read-string (str "[" s "]"))]
                                   [(conj x xi)
                                    (conj y yi)
                                    (conj z zi)]))
                               [[] [] []] t)))
                (apply concat))]
    (reduce (fn [cnt tri]
              (let [[c b a] (sort > tri)]
                (if (< c (+ a b))
                  (inc cnt)
                  cnt)))
            0 in)))

;; Entry point

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [in (slurp *in*)
        c (read-string (first args))
        f (case c
            1 one
            1.2 one-two
            2 (partial two button-map)
            2.2 (partial two button-map-2)
            3 three
            3.2 three-two)]
    (pprint (f in))))
