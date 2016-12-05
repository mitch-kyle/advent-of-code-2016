(ns advent2016.core
  (:require [advent2016.domain :as d]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

;; Load puzzles
(require 'advent2016.days-1-4)

(defn -main
  "First arg is puzzle number in the form day[.2]?
   puzzle input on stdin"
  [& args]
  (let [in (slurp *in*)
        n (read-string (first args))]
    (pprint (d/puzzle n in))))
