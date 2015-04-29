(defproject fourclojure-solutions "0.1.0-SNAPSHOT"
  :description "4Clojure solutions"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot fourclojure-solutions.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
