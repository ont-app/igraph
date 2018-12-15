(defproject ont-app/igraph "0.1.2"
  :description "Defines a IGraph protocol for maintaining a querying different graph implementations"
  :url "https://github.com/ont-app/igraph"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  ;; :main ^:skip-aot igraph.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
