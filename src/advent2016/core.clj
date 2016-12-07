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

;; Day six

(defmethod d/puzzle 6
  [_ in]
  (->> (string/split in #"\n")
       (apply map (fn [& cs]
                    (->> cs
                         frequencies
                         (sort-by last)
                         last
                         first)))
       (apply str)))

(defmethod d/puzzle 6.2
  [_ in]
  (->> (string/split in #"\n")
       (apply map (fn [& cs]
                    (->> cs
                         frequencies
                         (sort-by last)
                         first
                         first)))
       (apply str)))

;; Day seven

(defn has-abba?
  [s]
  (loop [stk (list)
         s (into (list) s)]
    (let [h (peek stk)
          c (peek s)]
      (cond
        (nil? c) false
        (not= c h) (recur (conj stk c)
                          (pop s))
        :else (let [s' (pop s)
                    h2 (peek (pop stk))
                    c2 (peek s')]
                (if (and (= c2 h2)
                         (not= c c2))
                  true
                  (recur (conj stk c)
                         s')))))))
(defn split-hypernets
  [s]
  (reduce (fn [[h nh] i]
            (if (.contains ^String i "]")
              (let [[he nhe] (string/split i #"\]")]
                [(conj h he) (conj nh nhe)])
              [h (conj nh i)]))
          [[] []]
          (string/split s #"\[")))

(defn has-tls?
  [s]
  (let [[hypn n-hypn] (split-hypernets s)]
    (and (some has-abba? n-hypn)
         (every? (comp not has-abba?) hypn))))

(defn babs
  "Get all the BABs for ABAs in the string"
  [s]
  (loop [res (list)
         stk (list)
         s (into (list) s)]
    (let [h (peek stk)
          s' (pop s)
          c (peek s)
          c' (peek s')]
      (cond
        (nil? c') res
        (or (not= c' h)
            (= c c'))   (recur res
                               (conj stk c)
                               s')
        :else (recur (conj res (str c h c))
                     (conj stk c)
                     s')))))

(defn has-ssl?
  [s]
  (let [[hypns n-hypns] (split-hypernets s)
        abas (->> n-hypns
                  (map babs)
                  flatten
                  (into #{}))]
    (when (seq abas)
      (some (fn [h]
              (some #(.contains ^String h %) abas)) hypns))))

(defmethod d/puzzle 7
  [_ in]
  (->> (string/split in #"\n")
       (filter has-tls?)
       count))

(defmethod d/puzzle 7.2
  [_ in]
  (->> (string/split in #"\n")
       (filter has-ssl?)
       count))

(defn -main
  "First arg is puzzle number in the form day[.2]?
   puzzle input on stdin"
  [& args]
  (let [in (slurp *in*)
        n (read-string (first args))]
    (pprint (d/puzzle n in))))
