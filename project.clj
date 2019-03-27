(defproject ont-app/igraph "0.1.4-SNAPSHOT"
  :description "Defines a IGraph protocol for maintaining a querying different graph implementations"
  :url "https://github.com/ont-app/igraph"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.227"]
                 ]

  ;; :main ^:skip-aot igraph.core
  :target-path "target/%s"
  :plugins [[lein-codox "0.10.6"]
            [lein-cljsbuild "1.1.7"
             :exclusions [[org.clojure/clojure]]]
            [lein-doo "0.1.10"]
            ]
  :cljsbuild
  {:builds
   ;; for testing the cljs incarnation
   ;; run with 'lein doo firefox test', or swap in some other browser
   {:test {:source-paths ["src" "cljs-test"]
           :compiler {:output-to "resources/test/compiled.js"
                      ;; entry point for doo-runner:
                      :main igraph.browser ;; at cljs-test/igraph/browser.cljs
                      :optimizations :none
                      :warnings {:bad-method-signature false}
                      }}}
   }
  :codox {:output-path "doc"}

  :profiles {:uberjar {:aot :all}})
