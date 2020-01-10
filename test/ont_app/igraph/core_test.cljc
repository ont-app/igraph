(ns ont-app.igraph.core-test
  (:require
   #?(:cljs [cljs.test :refer-macros [async deftest is testing]]
      :clj [clojure.test :refer :all])
   [ont-app.igraph.core :as igraph]))


;; TEST EXAMPLES IN THE README

(def subClassOf* (igraph/transitive-closure :subClassOf))

(def eg-data {:john {:isa #{:person}, :likes #{:beef}},
              :mary {:isa #{:person}, :likes #{:chicken}}})

(def eg (atom nil))
(def other-eg-data {:mary {:isa #{:person}, :likes #{:pork}},
                    :waldo {:isa #{:person}, :likes #{:beer}}})

(def other-eg (atom nil))
;; used to test IGraphSet operations.
;; Should contain other-eg-data.

(def types-data
  {
   :beef {:subClassOf #{:meat}},
   :chicken {:subClassOf #{:meat}}
   :pork {:subClassOf #{:meat}},
   :meat {:subClassOf #{:food}},
   :beer {:subClassOf #{:beverage}},
   :food {:subClassOf #{:consumable}},
   :beverage {:subClassOf #{:consumable}},
   :consumable {:subClassOf #{:thing}},
   :person {:subClassOf #{:thing}},
   })

(def eg-with-types (atom nil))


(deftest readme
  ;; all the examples in the README should work as advertised
  (when (and @eg @other-eg @eg-with-types)
    (testing "implementation independent"
      (is (= (igraph/triples-format {:john {:likes# #{:beef}}})
             :normal-form))
      (is (= (igraph/triples-format [:john :likes :beef])
             :vector))
      (is (= (igraph/triples-format [:john :isa :person :likes :beef])
             :vector))
      (is (= (igraph/triples-format  [[:john :isa :person] [:mary :isa :person]])
             :vector-of-vectors))
      (is (= (igraph/triples-removal-format [:john])
             :underspecified-triple))
      (is (= (igraph/triples-removal-format [:john :likes])
             :underspecified-triple))
      )
    (testing "eg-graph"
      (is (= (igraph/normal-form @eg) eg-data))
      (is (= (igraph/normal-form @eg)
             {:john {:isa #{:person}, :likes #{:beef}},
              :mary {:isa #{:person}, :likes #{:chicken}}}))
      (is (= (igraph/subjects @eg)
             #{:john :mary}))
      (is (= (igraph/get-p-o @eg :john)
             {:isa #{:person}, :likes #{:beef}}))
      (is (= (igraph/get-o @eg :john :isa)
             #{:person}))
      (is (not (nil? (igraph/ask @eg :john :likes :beef))))
      (is (= (igraph/ask @eg :john :likes :chicken)
             nil))
      (is (= (@eg)
             {:john {:isa #{:person}, :likes #{:beef}},
              :mary {:isa #{:person}, :likes #{:chicken}}}))
      (is (= (@eg :john)
             {:isa #{:person}, :likes #{:beef}}))
      (is (= (@eg :mary :likes)
             #{:chicken}))
      (is (not (nil? (@eg :mary :likes :chicken))))
      (is (nil? (@eg :mary :likes :beef)))
      (is (#{::igraph/read-only
             ::igraph/immutable
             ::igraph/mutable}
           (igraph/mutability @eg)))
      )
    (testing "IGraphImmutable"
      (when (= (igraph/mutability @eg) :igraph/immutable)
        (is (= (satisfies? igraph/IGraphImmutable @eg)
               true))
        (is (= (igraph/normal-form 
                (igraph/add 
                 @eg
                 [[:chicken :subclass-of :meat]
                  [:beef :subclass-of :meat]
                  ]))
               {:john {:isa #{:person}, :likes #{:beef}},
                :mary {:isa #{:person}, :likes #{:chicken}},
                :chicken {:subclass-of #{:meat}},
                :beef {:subclass-of #{:meat}}}))
        (is (= (igraph/normal-form (igraph/subtract @eg [:john]))
               {:mary {:isa #{:person}, :likes #{:chicken}}}))
        (is (= (igraph/normal-form (igraph/subtract @eg [:john :likes]))
               {:john {:isa #{:person}}, 
                :mary {:isa #{:person}, :likes #{:chicken}}}))
        
      )) ;; immutable
    (testing "IGraphMutable"
      (when (= (igraph/mutability @eg) :igraph/mutable)
        (is (= (satisfies? igraph/IGraphMutable @eg)
               true))))
    ;; TODO: add any future examples from README for mutable graphs
      
  (testing "IGraphSet"
    (when (satisfies? igraph/IGraphSet @eg)
      (is (= (igraph/normal-form @other-eg) other-eg-data))
      (is (= (satisfies? igraph/IGraphSet @other-eg)
             true))
      (is (= (igraph/normal-form (igraph/union @eg @other-eg))
             {:john {:isa #{:person}, :likes #{:beef}},
              :mary {:isa #{:person}, :likes #{:pork :chicken}},
              :waldo {:isa #{:person}, :likes #{:beer}}}))
      (is (= (igraph/normal-form (igraph/intersection @eg @other-eg))
             {:mary {:isa #{:person}}}))
      (is (= (igraph/normal-form (igraph/difference @eg @other-eg))
             {:john {:isa #{:person}, :likes #{:beef}},
              :mary {:likes #{:chicken}}}))
      (is (= (igraph/normal-form (igraph/difference @other-eg @eg))
             {:mary {:likes #{:pork}},
              :waldo {:isa #{:person}, :likes #{:beer}}}))))
  (testing "Traversal"
    (is (= (@eg-with-types)
           {:consumable {:subClassOf #{:thing}},
            :beef {:subClassOf #{:meat}},
            :person {:subClassOf #{:thing}},
            :beer {:subClassOf #{:beverage}},
            :meat {:subClassOf #{:food}},
            :food {:subClassOf #{:consumable}},
            :beverage {:subClassOf #{:consumable}},
            :pork {:subClassOf #{:meat}},
            :john {:isa #{:person}, :likes #{:beef}},
            :mary {:isa #{:person}, :likes #{:chicken}},
            :chicken {:subClassOf #{:meat}}}))
    (is (= (subClassOf* @eg-with-types {} #{} [:meat])
           [{} #{:meat} '(:food)]))
    (is (= (subClassOf* @eg-with-types {} #{:meat} '(:food))
           [{} #{:meat :food} '(:consumable)]))
    (is (= (igraph/traverse 
            @eg-with-types 
            (igraph/traverse-link :isa) 
            #{}
            [:john :mary])
           #{:person}))
    (is (= (igraph/traverse @eg-with-types 
                            (igraph/maybe-traverse-link :isa) 
                            #{} 
                            [:john :mary])
           #{:person :john :mary}))
    (let [subsumed-by (igraph/traverse-or :isa :subClassOf)]
      (is (= (igraph/traverse @eg-with-types subsumed-by #{} [:john])
             #{:person}))
      (is (= (igraph/traverse @eg-with-types subsumed-by #{} [:meat])
             #{:food}))
      )
    (let [instance-of (igraph/t-comp
                       [:isa (igraph/transitive-closure :subClassOf)])
          ]
      (is (= (igraph/traverse @eg-with-types instance-of #{} [:john])
             #{:person :thing})))
    
    (is (= (@eg-with-types :beef subClassOf*)
           #{:consumable :beef :meat :food :thing}))
    (is (= (@eg-with-types :beef subClassOf* :food)
           :food))
    (is (= (@eg-with-types :john (igraph/t-comp [:likes subClassOf*]))
           #{:consumable :beef :meat :food :thing}))
    ) ;; traversal
  (testing "Cardinality-1 utilities"
    (is (= (igraph/unique (@eg-with-types :john :isa))
           :person))
    (is (#{:consumable :beef :meat :food :thing}
         (igraph/unique (@eg-with-types :beef subClassOf*)
                        first)))
           
    (is (= (igraph/flatten-description (@eg-with-types :john))
           {:isa :person, :likes :beef}))
    (let [g (igraph/add 
             @eg 
             [:john :likes :beer :has-vector [1 2 3]])
        ]
      (is (= (igraph/flatten-description (g :john)))
          {:isa :person, :likes #{:beef :beer}, :has-vector [1 2 3]})
      
      (is (= (igraph/normalize-flat-description 
              {:isa :person, :likes #{:beef :beer}, :has-vector [1 2 3]})
             {:isa #{:person}, :likes #{:beef :beer}, :has-vector #{[1 2 3]}})))
    
    (let [g (igraph/add @eg {:john (igraph/normalize-flat-description
                                   {:likes :beer})})
          ]
      (is (= (g :john))
          {:isa #{:person}, :likes #{:beef :beer}}))
    
    (let [g (igraph/assert-unique @eg :john :isa :man)]
      (is (= (g :john)
             {:likes #{:beef}, :isa #{:man}})))

    ) ;; cardinality-1
  (testing "Other utilites"
    (letfn [(tally-triples [tally s p o]
                            (inc tally))
                            
            ]
      (is (= (igraph/reduce-spo tally-triples 0 @eg)
             4)))
    ) ;; other utilities
  ))
    


  
