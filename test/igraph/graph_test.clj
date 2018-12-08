(ns igraph.graph-test
  (:require [clojure.test :refer :all]
            [igraph.core :refer :all]
            [igraph.graph :refer :all]
            ))

(defonce test-graph (let [g (make-graph)
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
                         [:mary :name {:value "Mary" :lang "en"}]
                         [:john :name {:value "John" :lang "en"}]
                         ])))


(deftest invoke-test
  (testing "Invokes the test-graph as a function with different arities"
    (is (= (test-graph) ;; returns (normal-form g)
           {:john
            {:isa #{:person},
             :likes #{:meat},
             :name #{{:value "John", :lang "en"}}},
            :mary
            {:isa #{:person},
             :likes #{:coke},
             :name #{{:value "Mary", :lang "en"}}},
            :likes {:isa #{:property}},
            :isa {:isa #{:property}},
            :meat {:isa #{:food}},
            :coke {:isa #{:drink}}}))
    (is (= (test-graph :john) ;; returns (get-p-o g s)
           {:isa #{:person},
            :likes #{:meat},
            :name #{{:value "John", :lang "en"}}}))
    (is (= (test-graph :john :likes) ;; returns (get-o g s p)
           #{:meat}))
    (is (= (test-graph :john :likes :meat) ;; returns (ask g s p o)
           :meat))
  ))

(deftest query-test
  (testing "Tests a basic query against the dummy test-graph"
    (is (= (query test-graph
                  [[:?liker :likes :?likee]
                   [:?likee :isa :?type]])
           #{{:?type :drink,
              :?likee :coke,
              :?liker :mary}
             {:?type :food,
              :?likee :meat,
              :?liker :john}}
           ))))

(deftest utility-test
  (testing "Test the `unique` function"
    (is (= (unique #{:just-me})
           :just-me))
    (is (= (unique #{})
           nil))
    (is (thrown? Exception (unique #{:just-me :no-theres-me-too!})))

    (is (= (unique [:just-me :no-theres-me-too!] first)
           :just-me)
    )))
