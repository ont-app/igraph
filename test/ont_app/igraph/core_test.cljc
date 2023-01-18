(ns ont-app.igraph.core-test
  {
   ;; this metadata can be used by downstream libraries that rely on RDF URIs...
   :vann/preferredNamespacePrefix
   "igraph-test" ;; matches :: declarations directly
   :vann/preferredNamespaceUri
   "http://rdf.naturallexicon.org/ont-app/igraph/core-test#"
   }
  (:require
   #?(:cljs [cljs.test :refer-macros [deftest is testing]]
      :clj [clojure.test :refer :all])
   [ont-app.igraph.core :as igraph]
   [ont-app.igraph.graph :as native-normal]
   [ont-app.igraph.test-support :as ts]
   ))


;; READER MACROS ARE USED TO DISABLE TESTS THAT FAIL UNDER CLJS
;; These seem to be due to the fact that the `make-test-graph` is undefined
;; when passed in as the ts/makeGraphFn for some reason

;; TEST EXAMPLES IN THE README


(deftest readme-implementation-independent
  ;; all the examples in the README should work as advertised

    (testing "implementation independent"
      (is (= (igraph/triples-format
              {:igraph-test/john {:igraph-test/likes# #{:igraph-test/beef}}})
             :normal-form))
      (is (= (igraph/triples-format
              [:igraph-test/john :igraph-test/likes :igraph-test/beef])
             :vector))
      (is (= (igraph/triples-format
              [:igraph-test/john
               :igraph-test/isa :igraph-test/person :igraph-test/likes :igraph-test/beef])
             :vector))
      (is (= (igraph/triples-format
              [[:igraph-test/john :igraph-test/isa :igraph-test/person]
               [:igraph-test/mary :igraph-test/isa :igraph-test/person]])
             :vector-of-vectors))
      (is (= (igraph/triples-removal-format [:igraph-test/john])
             :underspecified-triple))
      (is (= (igraph/triples-removal-format [:igraph-test/john :igraph-test/likes])
             :underspecified-triple))))

(defn make-test-graph
  ^ont_app.igraph.graph.Graph [data]
  (native-normal/make-graph :contents data))

(defn make-standard-report
  "Returns a Standard IGraph Implementation Report with its makeGraphFn init'd to a native-normal graph."
  []
  (-> (native-normal/make-graph)
      (igraph/add [::ts/StandardIGraphImplementationReport
                   ::ts/makeGraphFn make-test-graph
                   ])))


(def the-report (atom nil))

;; The following tests are disabled under cljs pending resolution of issue #8

#?(:clj
   (deftest test-report-invalid-test-graph
     (testing "Null test graph"
            (let [report (ts/report-invalid-test-graph
                          (make-standard-report) nil
                          :test-fn-var #'test-report-invalid-test-graph
                          :protocol igraph/IGraph
                          :content-var #'ts/eg-data
                          :schema-graph nil)
                  ]
              (is (= (-> (ts/query-for-failures report)
                         igraph/unique
                         :?test
                         )
                     ::ts/TestGraphExistsTest))))
  
   (testing "Non-satisfactory test graph"
            (let [report (ts/report-invalid-test-graph
                          (make-standard-report) :not-a-graph
                          :test-fn-var #'test-report-invalid-test-graph
                          :protocol igraph/IGraph
                          :content-var #'ts/eg-data
                          :schema-graph nil)
                  ]
              (is (= (-> (ts/query-for-failures report)
                         igraph/unique
                         :?test
                         (name)
                         )
                     "TestGraphSatisfiesProtocolTest_ont_app.igraph.core.IGraph"
                     ))))

   (testing "Faulty contents test graph"
            (let [report (ts/report-invalid-test-graph
                          (make-standard-report) (native-normal/make-graph
                                                  :contents ts/other-eg-data)
                          :test-fn-var #'test-report-invalid-test-graph
                          :protocol igraph/IGraph
                          :content-var #'ts/eg-data
                          :schema-graph nil)
                  ]
              (is (= (-> (ts/query-for-failures report)
                         igraph/unique
                         :?test)
                     ::ts/TestGraphContentTest))))))
