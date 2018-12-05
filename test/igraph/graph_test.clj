(ns igraph.graph-test
  (:require [clojure.test :refer :all]
            [igraph.core :refer :all]
            [igraph.graph :refer :all]
            ))

(def test-graph (let [g (make-graph)
                      ]
                  (add g
                        [[:john :isa :person]
                         [:mary :isa :person]
                         [:likes :isa :property]
                         [:isa :isa :property]
                         [:john :likes :meat]
                         [:mary :likes :coke]
                         [:meat :isa :food]
                         [:coke :isa :drink]
                         [:mary :name {:value "mary" :lang "en"}]
                         [:john :name {:value "john" :lang "en"}]
                         ])))

(deftest query-test
  (testing "Tests a basic query against a dummy test-graph"
    (is (= (query test-graph
                  [[:?liker :likes :?likee]
                   [:?likee :isa :?type]])
           #{{:?type :drink, :?likee :coke, :?liker :mary} {:?type :food, :?likee :meat, :?liker :john}}))))

