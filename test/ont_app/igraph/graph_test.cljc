(ns ont-app.igraph.graph-test
  (:require
   #?(:cljs [cljs.test :refer-macros [async deftest is testing]]
      :clj [clojure.test :refer :all])
   [ont-app.igraph.core :as ig]
   ;; :refer [unique normal-form
   ;;                      add
   ;;                      transitive-closure]]
   [ont-app.igraph.graph :as g] ;;:refer [make-graph]]
   ))

;; (def test-graph (ig/add (g/make-graph) [[:a :b :c]]))
(def test-graph (ig/add (g/make-graph)
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
                      [:drink :subClassOf :consumable]
                      [:food :subClassOf :consumable]
                      [:consumable :subClassOf :thing]
                      [:person :subClassOf :thing]
                      ]))

^traversal-fn
(def subClassOf* (ig/transitive-closure :subClassOf))

^traversal-fn
(defn isa->subClassOf* [g context acc queue]
  [context
   (->> queue 
        (ig/traverse g (ig/traverse-link :isa)
                     (assoc (dissoc context :seek)
                            :phase :isa) #{})
        vec
        (ig/traverse g (ig/transitive-closure :subClassOf)
                     (assoc context :phase :sc)
                     #{}))
   []])

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
            :food {:subClassOf #{:consumable}},
            :coke {:isa #{:drink}},
            :drink {:subClassOf #{:consumable}},
            :consumable {:subClassOf #{:thing}},
            :person {:subClassOf #{:thing}}}
           ))
    (is (= (test-graph :john) ;; returns (get-p-o g s)
           {:isa #{:person},
            :likes #{:meat},
            :name #{{:value "John", :lang "en"}}}))
    (is (= (test-graph :john :likes) ;; returns (get-o g s p)
           #{:meat}))
    (is (= (test-graph :john :likes :meat) ;; returns (ask g s p o)
           :meat))
    (is (= (test-graph :drink subClassOf* :consumable)
           :consumable))
    (is (= (test-graph :drink subClassOf*)
           #{:consumable :drink :thing}))
    (is (= (test-graph :john (ig/transitive-closure
                              (ig/disjoint-traversal :isa :subClassOf)))
           ;; ... any sequence of isa|subClassOf
           #{:person :thing :john}))
    ))



(deftest subtract-test
  (testing "Variations on subtract"
    (is (= (ig/normal-form (ig/subtract test-graph [:john]))
           {:mary
            {:isa #{:person},
             :likes #{:coke},
             :name #{{:value "Mary", :lang "en"}}},
            :likes {:isa #{:property}},
            :isa {:isa #{:property}},
            :meat {:isa #{:food}},
            :food {:subClassOf #{:consumable}},
            :coke {:isa #{:drink}},
            :drink {:subClassOf #{:consumable}},
            :consumable {:subClassOf #{:thing}},
            :person {:subClassOf #{:thing}}}
           ))
    (is (= (ig/normal-form (ig/subtract test-graph [:john :likes]))
           {:john {:isa #{:person}, :name #{{:value "John", :lang "en"}}},
            :mary
            {:isa #{:person},
             :likes #{:coke},
             :name #{{:value "Mary", :lang "en"}}},
            :likes {:isa #{:property}},
            :isa {:isa #{:property}},
            :meat {:isa #{:food}},
            :food {:subClassOf #{:consumable}},
            :coke {:isa #{:drink}},
            :drink {:subClassOf #{:consumable}},
            :consumable {:subClassOf #{:thing}},
            :person {:subClassOf #{:thing}}}
           ))

    (is (= (ig/normal-form (ig/subtract test-graph [:john :likes :meat]))
           {:john {:isa #{:person}, :name #{{:value "John", :lang "en"}}},
            :mary
            {:isa #{:person},
             :likes #{:coke},
             :name #{{:value "Mary", :lang "en"}}},
            :likes {:isa #{:property}},
            :isa {:isa #{:property}},
            :meat {:isa #{:food}},
            :food {:subClassOf #{:consumable}},
            :coke {:isa #{:drink}},
            :drink {:subClassOf #{:consumable}},
            :consumable {:subClassOf #{:thing}},
            :person {:subClassOf #{:thing}}}
           ))
    (is (= (ig/normal-form (ig/subtract test-graph [[:john
                                               :likes :meat
                                               :isa :person]
                                              [:mary :name]]))
           {:john {:name #{{:value "John", :lang "en"}}},
            :mary {:isa #{:person}, :likes #{:coke}},
            :likes {:isa #{:property}},
            :isa {:isa #{:property}},
            :meat {:isa #{:food}},
            :food {:subClassOf #{:consumable}},
            :coke {:isa #{:drink}},
            :drink {:subClassOf #{:consumable}},
            :consumable {:subClassOf #{:thing}},
            :person {:subClassOf #{:thing}}}
           
           ))
    ))

(deftest query-test
  (testing "Tests a basic query against the dummy test-graph"
    (is (= (ig/query test-graph
                  [[:?liker :likes :?likee]
                   [:?likee :isa :?type]]
                  )
           #{{:?type :drink,
              :?likee :coke,
              :?liker :mary}
             {:?type :food,
              :?likee :meat,
              :?liker :john}}
           ))
    (is (= (ig/query test-graph
                  [[:?liker :likes :?likee]
                   [:?likee :isa :?class]
                   [:?class subClassOf* :?super]])
           #{{:?super :thing, :?class :food, :?likee :meat, :?liker :john}
             {:?super :thing, :?class :drink, :?likee :coke, :?liker :mary}
             {:?super :food, :?class :food, :?likee :meat, :?liker :john}
             {:?super :consumable, :?class :drink, :?likee :coke, :?liker :mary}
             {:?super :consumable, :?class :food, :?likee :meat, :?liker :john}
             {:?super :drink, :?class :drink, :?likee :coke, :?liker :mary}}))
    
    ;; transitive closure should include the o-spec...
    (is (= (ig/query test-graph [[:?s (ig/transitive-closure :isa) :property]])
           #{{:?s :likes} {:?s :isa} {:?s :property}}))
    ))

(deftest utility-test
  (testing "Test the `unique` function"
    (is (= (ig/unique #{:just-me})
           :just-me))
    (is (= (ig/unique #{})
           nil))

    (is (thrown? #?(:cljs js/Object :clj Exception)
                 (ig/unique #{:just-me :no-theres-me-too!})))

    (is (= (ig/unique [:just-me :no-theres-me-too!] first)
           :just-me))
   
    ;; specific to Graph, not part of IGraph
    (is (= (set (g/vector-of-triples test-graph))
           #{[:consumable :subClassOf :thing]
             [:person :subClassOf :thing]
             [:isa :isa :property]
             [:drink :subClassOf :consumable]
             [:likes :isa :property]
             [:coke :isa :drink]
             [:meat :isa :food]
             [:food :subClassOf :consumable]
             [:john :isa :person]
             [:john :likes :meat]
             [:john :name {:value "John", :lang "en"}]
             [:mary :isa :person]
             [:mary :likes :coke]
             [:mary :name {:value "Mary", :lang "en"}]}))
    ))

(deftest iset-test
  (testing "Test the ISet functions"
    (let [g1 (ig/add (g/make-graph) [[:a :b :c :d :e]
                                     [:f :g :h :i :j]
                                     ])
          g2 (ig/add (g/make-graph) [[:a :b :c]
                                     [:x :y :z]])
          ]
                               
      (is (= (ig/normal-form (ig/union g1 g2))
             (ig/normal-form (ig/add (g/make-graph) [[:a :b :c :d :e]
                                                     [:f :g :h :i :j]
                                                     [:x :y :z]
                                                     ]))))
      (is (= (ig/normal-form (ig/intersection g1 g2))
             (ig/normal-form (ig/add (g/make-graph)
                                     [[:a :b :c]]))))
      (is (= (ig/normal-form (ig/difference g1 g2))
             (ig/normal-form (ig/add (g/make-graph)
                                     [[:a :d :e]
                                      [:f :g :h :i :j]]))))
      )))

(deftest traverse-test
  (testing "Test traverse and transitive-closure"
    (let [g (ig/add (g/make-graph)
                    [[:a :isa :b] [:b :isa :c][:c :isa :d]])
          isa* (fn [g context acc to-visit]
                 [context,
                  (conj acc (first to-visit)),
                  (concat (rest to-visit) (g (first to-visit) :isa))])
          ]
      (is (= (ig/traverse g isa* [] [:a])
             [:a :b :c :d]))
      ;; transitive-closure writes equivalent of isa*...
      (is (= (ig/traverse g (ig/transitive-closure :isa) [] [:a])
             [:a :b :c :d]))
      
      ;; traversal p's
      (is (= (test-graph :coke (ig/traverse-link :isa))
             #{:drink}))
      (is (= (test-graph :coke (ig/traverse-link :isa) :drink)
             :drink))
      (is (= (test-graph :drink subClassOf*)
             #{:consumable :drink :thing}))
      (is (= (test-graph :coke isa->subClassOf* :consumable)
             :consumable))
      (is (= (test-graph :coke isa->subClassOf* :drink)
             :drink))
      (is (= (test-graph :coke isa->subClassOf* :person)
             nil))      
      )))
