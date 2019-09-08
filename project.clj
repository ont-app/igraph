(defproject ont-app/igraph "0.1.4-SNAPSHOT"
  :description "Defines a IGraph protocol for maintaining a querying different graph implementations"
  :url "https://github.com/ont-app/igraph"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.227"]
                 [lein-doo "0.1.10"]
                 ]
  
  ;; :main ^:skip-aot ont-app.igraph.core
  :target-path "target/%s"
  :resource-paths ["resources" "target/cljsbuild"]
  
  :plugins [[lein-codox "0.10.6"]
            [lein-cljsbuild "1.1.7"
             :exclusions [[org.clojure/clojure]]]
            [lein-doo "0.1.10"]
            ]
  :source-paths ["src"]
  :test-paths ["src" "test"]
  :cljsbuild
  {
   :test-commands {"test" ["lein" "doo" "node" "test" "once"]}
   :builds
   {
    :dev {:source-paths ["src"]
           :compiler {
                      :main ont-app.igraph.core 
                      :asset-path "js/compiled/out"
                      :output-to "resources/public/js/igraph.js"
                      :source-map-timestamp true
                      :output-dir "resources/public/js/compiled/out"
                      :optimizations :none
                      }
          }
    ;; for testing the cljs incarnation
    ;; run with 'lein doo node test
    :test {:source-paths ["src" "test"]
           :compiler {
                      :main ont-app.igraph.doo
                      :target :nodejs
                      :asset-path "resources/test/js/compiled/out"
                      :output-to "resources/test/compiled.js"
                      :output-dir "resources/test/js/compiled/out"
                      :optimizations :advanced ;;:none
                      }
           }
   }} ;; end cljsbuild

  :codox {:output-path "doc"}

  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[binaryage/devtools "0.9.10"]
                                  ]
                   :source-paths ["src"] 
                   :clean-targets
                   ^{:protect false}
                   ["resources/public/js/compiled"
                    "resources/test"
                    :target-path
                    ]
                   }
             })
