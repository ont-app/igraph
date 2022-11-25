(ns build
  "Adpated from https://github.com/seancorfield/deps-new/blob/develop/resources/org/corfield/new/lib/root/build.clj"
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b] ; for b/git-count-revs
            [org.corfield.build :as bb]))

(def lib 'ont-app/vocabulary)

(def version "0.2.0-SNAPSHOT")

(defn test "Run the tests." [opts]
  (bb/run-tests opts))

(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/run-tests)
      (bb/clean)
      (bb/jar)))

(defn clean "Cleans any clj/s compilation output.
  Where:
  `opts` := `m` s.t. (keys m) may match #{:include-caches?, ...}
  `include-caches?` when truthy indicates to clear .cpcache and .shadow-cljs directories.
  "
  [opts]
  (println (str "Cleaning with opts:" opts "."))
  ;; TODO: check opts
  (bb/clean opts)
  (b/delete {:path "./out"})  
  (b/delete {:path "./cljs-test-runner-out"})
  (when (:include-caches? opts)
    (println (str "Clearing caches"))
    (b/delete {:path "./.cpcache"})  
    (b/delete {:path "./.shadow-cljs"}))
  opts)

(defn install "Install the JAR locally." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/install)))

(defn deploy
  "Deploy the JAR to Clojars. Using $CLOJARS_USERNAME and $CLOJARS_PASSWORD"
  [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/deploy)))

