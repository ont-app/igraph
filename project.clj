(defproject ont-app/igraph "0.1.6"
  :description "Defines a IGraph protocol for maintaining a querying different graph implementations"
  :url "https://github.com/ont-app/igraph"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/clojurescript "1.10.879"
                  :exclusions [com.google.errorprone/error_prone_annotations
                               com.google.code.findbugs/jsr305
                               ]] 
                 [org.clojure/spec.alpha "0.2.194"]
                 [lein-doo "0.1.11"]
                 ]
  
  ;; :main ^:skip-aot ont-app.igraph.core
  :plugins [[lein-codox "0.10.6"]
            [lein-cljsbuild "1.1.7"
             :exclusions [[org.clojure/clojure]]]
            [lein-doo "0.1.11"]
            ]
  :target-path "target/%s"
  :resource-paths ["resources" "target/cljsbuild"]
  :source-paths ["src" "test"] ;; test files inform implementation tests downstream
  :test-paths ["src" "test"]
  :cljsbuild
  {
   :test-commands {"test" ["lein" "doo" "node" "test" "once"]}
   :builds
   {
    :dev {:source-paths ["src"]
           :compiler {
                      :main ont-app.igraph.core 
                      :output-to "resources/dev/js/igraph.js"
                      :source-map-timestamp true
                      :output-dir "resources/dev/js/compiled/out"
                      :optimizations :none
                      }
          }
    ;; for testing the cljs incarnation
    ;; run with 'lein doo node test
    :test {:source-paths ["src" "test"]
           :compiler {
                      :main ont-app.igraph.doo
                      :target :nodejs
                      :output-to "resources/test/compiled.js"
                      :output-dir "resources/test/js/compiled/out"
                      :optimizations :advanced ;;:none
                      :warnings {:bad-method-signature false}
                      }
           }

   }} ;; end cljsbuild

  :codox {:output-path "doc"}

  :profiles {:uberjar {}
             :dev {:dependencies [[binaryage/devtools "1.0.3"]
                                  ]
                   :source-paths ["src"] 
                   :clean-targets
                   ^{:protect false}
                   ["resources/dev/js/compiled"
                    "resources/test"
                    :target-path
                    ]
                   }
             })
