(ns igraph.graph-test
  (:require [clojure.test :refer :all]
            [igraph.core :refer :all]
            [igraph.graph :refer :all]
            ))

(defonce test-graph (add (make-graph)
                         [[:john :isa :person]
                          [:john :likes :meat]
                          [:john :name {:value "John" :lang "en"}]
                          [:mary
                           :isa :person
                           :likes :coke
                           :name {:value "Mary" :lang "en"}
                           ]
                          [:likes :isa :property]
                          [:isa :isa :property]
                          [:meat :isa :food]
                          [:coke :isa :drink]
                          ]))


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

(deftest subtract-test
  (testing "Variations on subtract"
    (is (= (normal-form (subtract test-graph [:john]))
           {:mary
            {:isa #{:person},
             :likes #{:coke},
             :name #{{:value "Mary", :lang "en"}}},
            :likes {:isa #{:property}},
            :isa {:isa #{:property}},
            :meat {:isa #{:food}},
            :coke {:isa #{:drink}}}))
    (is (= (normal-form (subtract test-graph [:john :likes]))
           {:john {:isa #{:person}, :name #{{:value "John", :lang "en"}}},
            :mary
            {:isa #{:person},
             :likes #{:coke},
             :name #{{:value "Mary", :lang "en"}}},
            :likes {:isa #{:property}},
            :isa {:isa #{:property}},
            :meat {:isa #{:food}},
            :coke {:isa #{:drink}}}))
    (is (= (normal-form (subtract test-graph [:john :likes :meat]))
           {:john {:isa #{:person}, :name #{{:value "John", :lang "en"}}},
            :mary
            {:isa #{:person},
             :likes #{:coke},
             :name #{{:value "Mary", :lang "en"}}},
            :likes {:isa #{:property}},
            :isa {:isa #{:property}},
            :meat {:isa #{:food}},
            :coke {:isa #{:drink}}}))
    (is (= (normal-form (subtract test-graph [[:john
                                               :likes :meat
                                               :isa :person]
                                              [:mary :name]]))
           {:john {:name #{{:value "John", :lang "en"}}},
            :mary {:isa #{:person}, :likes #{:coke}},
            :likes {:isa #{:property}},
            :isa {:isa #{:property}},
            :meat {:isa #{:food}},
            :coke {:isa #{:drink}}}))
    ))

(deftest query-test
  (testing "Tests a basic query against the dummy test-graph"
    (is (= (query test-graph
                  [[:?liker :likes :?likee]
                   [:?likee :isa :?type]]
                  )
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

(deftest iset-test
  (testing "Test the ISet functions"
    (let [g1 (add (make-graph) [[:a :b :c :d :e]
                                [:f :g :h :i :j]
                                ])
          g2 (add (make-graph) [[:a :b :c]
                                [:x :y :z]])
          ]
                               
    (is (= (normal-form (union g1 g2))
           (normal-form (add (make-graph) [[:a :b :c :d :e]
                                           [:f :g :h :i :j]
                                           [:x :y :z]
                                           ]))))
    (is (= (normal-form (intersection g1 g2))
           (normal-form (add (make-graph)
                             [[:a :b :c]]))))
    (is (= (normal-form (difference g1 g2))
           (normal-form (add (make-graph)
                             [[:a :d :e]
                              [:f :g :h :i :j]]))))
    )))

(deftest traverse-test
  (testing "Test traverse and transitive-closure"
    (let [g (add (make-graph) [[:a :isa :b] [:b :isa :c][:c :isa :d]])
          isa* (fn [g acc to-visit]
                 [(conj acc (first to-visit))
                  (concat (rest to-visit) (g (first to-visit) :isa))])
          ]
      (is (= (traverse g isa* [] [:a])
             [:a :b :c :d]))
      ;; transitive-closure writes equivalent of isa*...
      (is (= (traverse g (transitive-closure :isa) [] [:a])
             [:a :b :c :d])))))
      
      
