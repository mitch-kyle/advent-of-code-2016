(defproject advent2016 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [digest "1.4.5"]]
  :main ^:skip-aot advent2016.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
