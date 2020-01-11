(ns ont-app.igraph.graph-test
  (:require
   #?(:cljs [cljs.test :refer-macros [async deftest is testing]]
      :clj [clojure.test :refer :all])
   [ont-app.igraph.core :as igraph]
   ;; :refer [unique normal-form
   ;;                      add
   ;;                      transitive-closure]]
   [ont-app.igraph.graph :as g] ;;:refer [make-graph]]
   [ont-app.igraph.core-test :as ct]
   ))


;; README EXAMPLES
(reset! ct/eg
        (g/make-graph :contents ct/eg-data))

(reset! ct/other-eg
        (g/make-graph :contents ct/other-eg-data))

(reset! ct/eg-with-types
        (igraph/add @ct/eg ct/types-data))
                        

(deftest readme-examples
  (testing "core test readme"
    (ct/readme)
    (ct/readme-immutable)
    (ct/readme-set-operations))
  (testing "query"
    (is (= (igraph/query @ct/eg [[:?person ::ct/isa ::ct/person]])
           #{{:?person ::ct/mary} {:?person ::ct/john}})))
  )


;; (def test-graph (igraph/add (g/make-graph) [[:a :b :c]]))
(def test-graph (igraph/add (g/make-graph)
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
(def subClassOf* (igraph/transitive-closure :subClassOf))

^traversal-fn
(defn isa->subClassOf* [g context acc queue]
  [context
   (->> queue 
        (igraph/traverse g (igraph/traverse-link :isa)
                     (assoc (dissoc context :seek)
                            :phase :isa) #{})
        vec
        (igraph/traverse g (igraph/transitive-closure :subClassOf)
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
    (is (= (test-graph :john (igraph/transitive-closure
                              (igraph/traverse-or :isa :subClassOf)))
           ;; ... (isa|subClassOf)*
           #{:person :thing :john}))

    ))


(deftest subtract-test
  (testing "Variations on subtract"
    (is (= (igraph/normal-form (igraph/subtract test-graph [:john]))
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
    (is (= (igraph/normal-form (igraph/subtract test-graph [:john :likes]))
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

    (is (= (igraph/normal-form (igraph/subtract test-graph [:john :likes :meat]))
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
    (is (= (igraph/normal-form (igraph/subtract test-graph [[:john
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
    (is (= (igraph/query test-graph
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
    (is (= (igraph/query test-graph
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
    (is (= (igraph/query test-graph [[:?s (igraph/transitive-closure :isa) :property]])
           #{{:?s :likes} {:?s :isa} {:?s :property}}))
    ))

(deftest utility-test
  (testing "Test the `unique` function"
    (is (= (igraph/unique #{:just-me})
           :just-me))
    (is (= (igraph/unique #{})
           nil))

    (is (thrown? #?(:cljs js/Object :clj Exception)
                 (igraph/unique #{:just-me :no-theres-me-too!})))

    (is (= (igraph/unique [:just-me :no-theres-me-too!] first)
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
    (let [g1 (igraph/add (g/make-graph) [[:a :b :c :d :e]
                                     [:f :g :h :i :j]
                                     ])
          g2 (igraph/add (g/make-graph) [[:a :b :c]
                                     [:x :y :z]])
          ]
                               
      (is (= (igraph/normal-form (igraph/union g1 g2))
             (igraph/normal-form (igraph/add (g/make-graph) [[:a :b :c :d :e]
                                                     [:f :g :h :i :j]
                                                     [:x :y :z]
                                                     ]))))
      (is (= (igraph/normal-form (igraph/intersection g1 g2))
             (igraph/normal-form (igraph/add (g/make-graph)
                                     [[:a :b :c]]))))
      (is (= (igraph/normal-form (igraph/difference g1 g2))
             (igraph/normal-form (igraph/add (g/make-graph)
                                     [[:a :d :e]
                                      [:f :g :h :i :j]]))))
      )))

(deftest traverse-test
  (testing "Test traverse and transitive-closure"
    (let [g (igraph/add (g/make-graph)
                    [[:a :isa :b] [:b :isa :c][:c :isa :d]])
          isa* (fn [g context acc to-visit]
                 [context,
                  (conj acc (first to-visit)),
                  (concat (rest to-visit) (g (first to-visit) :isa))])
          ]
      (is (= (igraph/traverse g isa* [] [:a])
             [:a :b :c :d]))
      ;; transitive-closure writes equivalent of isa*...
      (is (= (igraph/traverse g (igraph/transitive-closure :isa) [] [:a])
             [:a :b :c :d]))
      
      ;; traversal p's
      (is (= (test-graph :coke (igraph/traverse-link :isa))
             #{:drink}))
      (is (= (test-graph :coke (igraph/traverse-link :isa) :drink)
             :drink))
      (is (= (test-graph :drink subClassOf*)
             #{:consumable :drink :thing}))
      (is (= (test-graph :coke isa->subClassOf* :consumable)
             :consumable))
      (is (= (test-graph :coke isa->subClassOf* :drink)
             :drink))
      (is (= (test-graph :coke isa->subClassOf* :person)
             nil))
      (is (= (test-graph :john (igraph/t-comp [:likes :isa]))
             #{:food}))
      )))

#?(:clj
   (deftest io-test
     (testing "Saving and restoring"
       (let [test-path (igraph/write-to-file "/tmp/igraph-test.edn" test-graph)
             test-graph' (igraph/read-from-file (g/make-graph) test-path)
             ]
         (is (.exists (clojure.java.io/as-file test-path))
             true)
         (is (= (igraph/normal-form test-graph')
                (igraph/normal-form test-graph)))
         (clojure.java.io/delete-file test-path)))))
         
         
