(ns advent2016.core
  (:require [advent2016.domain :as d]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [digest])
  (:gen-class))

;; Load puzzles
(require 'advent2016.days-1-4)

;; Day five

;; We're brute forcin' it
(defn password-chars
  ([f s] (password-chars f s 0))
  ([f s n] (loop [n n]
             (let [hash (digest/md5 (str s n))]
               (if (.startsWith ^String hash "00000")
                 (lazy-seq (cons (f hash)
                                 (password-chars f s (inc n))))
                 (recur (inc n)))))))

(defmethod d/puzzle 5
  [_ in]
  (->> (string/replace in #"\n" "")
       (password-chars #(get % 5))
       (take 8)
       (apply str)))

(defmethod d/puzzle 5.2
  [_ in]
  (let [chars (password-chars #(list (read-string (str (get % 5)))
                                     (get % 6))
                              (string/replace in #"\n" ""))]
    (->> (range 8)
         (map (fn [x]
                (->> chars
                     (some #(when (= x (first %))
                              %))
                     last)))
         (apply str))))
         
(defn -main
  "First arg is puzzle number in the form day[.2]?
   puzzle input on stdin"
  [& args]
  (let [in (slurp *in*)
        n (read-string (first args))]
    (pprint (d/puzzle n in))))
