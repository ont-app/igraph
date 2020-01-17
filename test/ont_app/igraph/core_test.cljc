(ns ont-app.igraph.core-test
  {
   ;; this metadata can be used by downstream libraries that rely on RDF URIs...
   :vann/preferredNamespacePrefix
   "ig-ctest" ;; matches :: declarations directly
   :vann/preferredNamespaceUri
   "http://rdf.naturallexicon.org/ont-app/igraph/core-test#"
   }
  (:require
   #?(:cljs [cljs.test :refer-macros [async deftest is testing]]
      :clj [clojure.test :refer :all])
   [ont-app.igraph.core :as igraph]))


;; FUN WITH READER MACROS
(def cljs-LazySeq #?(:clj clojure.lang.LazySeq
                     :cljs cljs.core/LazySeq))

;; No reader macros below this point

;; TEST EXAMPLES IN THE README

(def subClassOf* (igraph/transitive-closure :ig-ctest/subClassOf))

(def eg-data "Initial data for the `eg` graph"
  {:ig-ctest/john
   {:ig-ctest/isa #{:ig-ctest/person}, :ig-ctest/likes #{:ig-ctest/beef}},
   :ig-ctest/mary
   {:ig-ctest/isa #{:ig-ctest/person}, :ig-ctest/likes #{:ig-ctest/chicken}}})

(def eg "Holds examples from the README"
  (atom nil))

(def other-eg-data "Contents of the `other-eg` graph"
  {:ig-ctest/mary
   {:ig-ctest/isa #{:ig-ctest/person}, :ig-ctest/likes #{:ig-ctest/pork}},
   :ig-ctest/waldo
   {:ig-ctest/isa #{:ig-ctest/person}, :ig-ctest/likes #{:ig-ctest/beer}}})

(def other-eg
  "Used to deftest set operations per the README.
  Populated with `other-eg-data`"
  (atom nil))


(def types-data
  "Contents of the `eg-with-types` graph"
  {
   :ig-ctest/beef {:ig-ctest/subClassOf #{:ig-ctest/meat}},
   :ig-ctest/chicken {:ig-ctest/subClassOf #{:ig-ctest/meat}}
   :ig-ctest/pork {:ig-ctest/subClassOf #{:ig-ctest/meat}},
   :ig-ctest/meat {:ig-ctest/subClassOf #{:ig-ctest/food}},
   :ig-ctest/beer {:ig-ctest/subClassOf #{:ig-ctest/beverage}},
   :ig-ctest/food {:ig-ctest/subClassOf #{:ig-ctest/consumable}},
   :ig-ctest/beverage {:ig-ctest/subClassOf #{:ig-ctest/consumable}},
   :ig-ctest/consumable {:ig-ctest/subClassOf #{:ig-ctest/thing}},
   :ig-ctest/person {:ig-ctest/subClassOf #{:ig-ctest/thing}},
   })

(def eg-with-types
  "Used to test examples from the README"
  (atom nil))


(deftest readme
  ;; all the examples in the README should work as advertised
  (when (and @eg @other-eg @eg-with-types)
    (testing "implementation independent"
      (is (= (igraph/triples-format
              {:ig-ctest/john {:ig-ctest/likes# #{:ig-ctest/beef}}})
             :normal-form))
      (is (= (igraph/triples-format
              [:ig-ctest/john :ig-ctest/likes :ig-ctest/beef])
             :vector))
      (is (= (igraph/triples-format
              [:ig-ctest/john
               :ig-ctest/isa :ig-ctest/person :ig-ctest/likes :ig-ctest/beef])
             :vector))
      (is (= (igraph/triples-format
              [[:ig-ctest/john :ig-ctest/isa :ig-ctest/person]
               [:ig-ctest/mary :ig-ctest/isa :ig-ctest/person]])
             :vector-of-vectors))
      (is (= (igraph/triples-removal-format [:ig-ctest/john])
             :underspecified-triple))
      (is (= (igraph/triples-removal-format [:ig-ctest/john :ig-ctest/likes])
             :underspecified-triple))
      )
    (testing "eg-graph"
      (is (= (igraph/normal-form @eg) eg-data))
      (is (= (igraph/normal-form @eg)
             {:ig-ctest/john {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/beef}},
              :ig-ctest/mary {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/chicken}}}))
      (is (= (sort (igraph/subjects @eg))
             `(:ig-ctest/john :ig-ctest/mary)))
      (is (= (type (igraph/subjects @eg))
             cljs-LazySeq))
      (is (= (igraph/get-p-o @eg :ig-ctest/john)
             {:ig-ctest/isa #{:ig-ctest/person},
              :ig-ctest/likes #{:ig-ctest/beef}}))
      (is (= (igraph/get-o @eg :ig-ctest/john :ig-ctest/isa)
             #{:ig-ctest/person}))
      (is (not (nil? (igraph/ask @eg
                                 :ig-ctest/john
                                 :ig-ctest/likes
                                 :ig-ctest/beef))))
      (is (= (igraph/ask @eg :ig-ctest/john :ig-ctest/likes :ig-ctest/chicken)
             nil))
      (is (= (@eg)
             {:ig-ctest/john {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/beef}},
              :ig-ctest/mary {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/chicken}}}))
      (is (= (@eg :ig-ctest/john)
             {:ig-ctest/isa #{:ig-ctest/person},
              :ig-ctest/likes #{:ig-ctest/beef}}))
      (is (= (@eg :ig-ctest/mary :ig-ctest/likes)
             #{:ig-ctest/chicken}))
      (is (not (nil? (@eg :ig-ctest/mary :ig-ctest/likes :ig-ctest/chicken))))
      (is (nil? (@eg :ig-ctest/mary :ig-ctest/likes :ig-ctest/beef)))
      (is (#{::igraph/read-only
             ::igraph/immutable
             ::igraph/mutable}
           (igraph/mutability @eg)))
      )
    (testing "IGraphMutable"
      (when (= (igraph/mutability @eg) :igraph/mutable)
        (is (= (satisfies? igraph/IGraphMutable @eg)
               true))))
    ;; TODO: add any future examples from README for mutable graphs
      
  (testing "Traversal"
    (is (= (@eg-with-types)
           {:ig-ctest/consumable {:ig-ctest/subClassOf #{:ig-ctest/thing}},
            :ig-ctest/beef {:ig-ctest/subClassOf #{:ig-ctest/meat}},
            :ig-ctest/person {:ig-ctest/subClassOf #{:ig-ctest/thing}},
            :ig-ctest/beer {:ig-ctest/subClassOf #{:ig-ctest/beverage}},
            :ig-ctest/meat {:ig-ctest/subClassOf #{:ig-ctest/food}},
            :ig-ctest/food {:ig-ctest/subClassOf #{:ig-ctest/consumable}},
            :ig-ctest/beverage {:ig-ctest/subClassOf #{:ig-ctest/consumable}},
            :ig-ctest/pork {:ig-ctest/subClassOf #{:ig-ctest/meat}},
            :ig-ctest/john {:ig-ctest/isa #{:ig-ctest/person},
                            :ig-ctest/likes #{:ig-ctest/beef}},
            :ig-ctest/mary {:ig-ctest/isa #{:ig-ctest/person},
                            :ig-ctest/likes #{:ig-ctest/chicken}},
            :ig-ctest/chicken {:ig-ctest/subClassOf #{:ig-ctest/meat}}}))
    (is (= (subClassOf* @eg-with-types {} #{} [:ig-ctest/meat])
           [{} #{:ig-ctest/meat} '(:ig-ctest/food)]))
    (is (= (subClassOf* @eg-with-types {} #{:ig-ctest/meat} '(:ig-ctest/food))
           [{} #{:ig-ctest/meat :ig-ctest/food} '(:ig-ctest/consumable)]))
    (is (= (igraph/traverse 
            @eg-with-types 
            (igraph/traverse-link :ig-ctest/isa) 
            #{}
            [:ig-ctest/john :ig-ctest/mary])
           #{:ig-ctest/person}))
    (is (= (igraph/traverse @eg-with-types 
                            (igraph/maybe-traverse-link :ig-ctest/isa) 
                            #{} 
                            [:ig-ctest/john :ig-ctest/mary])
           #{:ig-ctest/person :ig-ctest/john :ig-ctest/mary}))
    (let [subsumed-by (igraph/traverse-or :ig-ctest/isa :ig-ctest/subClassOf)]
      (is (= (igraph/traverse @eg-with-types subsumed-by #{} [:ig-ctest/john])
             #{:ig-ctest/person}))
      (is (= (igraph/traverse @eg-with-types subsumed-by #{} [:ig-ctest/meat])
             #{:ig-ctest/food}))
      )
    (let [instance-of (igraph/t-comp
                       [:ig-ctest/isa
                        (igraph/transitive-closure :ig-ctest/subClassOf)])
          ]
      (is (= (igraph/traverse @eg-with-types instance-of #{} [:ig-ctest/john])
             #{:ig-ctest/person :ig-ctest/thing})))
    
    (is (= (@eg-with-types :ig-ctest/beef subClassOf*)
           #{:ig-ctest/consumable :ig-ctest/beef
             :ig-ctest/meat
             :ig-ctest/food :ig-ctest/thing}))
    (is (= (@eg-with-types :ig-ctest/beef subClassOf* :ig-ctest/food)
           :ig-ctest/food))
    (is (= (@eg-with-types :ig-ctest/john (igraph/t-comp
                                           [:ig-ctest/likes subClassOf*]))
           #{:ig-ctest/consumable
             :ig-ctest/beef :ig-ctest/meat :ig-ctest/food :ig-ctest/thing}))
    ) ;; traversal
  (testing "Cardinality-1 utilities"
    (is (= (igraph/unique (@eg-with-types :ig-ctest/john :ig-ctest/isa))
           :ig-ctest/person))
    (is (#{:ig-ctest/consumable
           :ig-ctest/beef :ig-ctest/meat :ig-ctest/food :ig-ctest/thing}
         (igraph/unique (@eg-with-types :ig-ctest/beef subClassOf*)
                        first)))
           
    (is (= (igraph/flatten-description (@eg-with-types :ig-ctest/john))
           {:ig-ctest/isa :ig-ctest/person, :ig-ctest/likes :ig-ctest/beef}))
    (let [g (igraph/add 
             @eg 
             [:ig-ctest/john
              :ig-ctest/likes :ig-ctest/beer :ig-ctest/has-vector [1 2 3]])
          ]
      (is (= (igraph/flatten-description (g :ig-ctest/john)))
          {:ig-ctest/isa :ig-ctest/person,
           :ig-ctest/likes #{:ig-ctest/beef :ig-ctest/beer},
           :ig-ctest/has-vector [1 2 3]})
      
      (is (= (igraph/normalize-flat-description 
              {:ig-ctest/isa :ig-ctest/person,
               :ig-ctest/likes #{:ig-ctest/beef :ig-ctest/beer},
               :ig-ctest/has-vector [1 2 3]})
             {:ig-ctest/isa #{:ig-ctest/person},
              :ig-ctest/likes #{:ig-ctest/beef :ig-ctest/beer},
              :ig-ctest/has-vector #{[1 2 3]}})))
    
    (let [g (igraph/add @eg {:ig-ctest/john (igraph/normalize-flat-description
                                   {:ig-ctest/likes :ig-ctest/beer})})
          ]
      (is (= (g :ig-ctest/john))
          {:ig-ctest/isa #{:ig-ctest/person},
           :ig-ctest/likes #{:ig-ctest/beef :ig-ctest/beer}}))
    
    (let [g (igraph/assert-unique @eg
                                  :ig-ctest/john :ig-ctest/isa :ig-ctest/man)]
      (is (= (g :ig-ctest/john)
             {:ig-ctest/likes #{:ig-ctest/beef},
              :ig-ctest/isa #{:ig-ctest/man}})))

    ) ;; cardinality-1
  (testing "Other utilites"
    (letfn [(tally-triples [tally s p o]
                            (inc tally))
                            
            ]
      (is (= (igraph/reduce-spo tally-triples 0 @eg)
             4)))
    ) ;; other utilities
  ))


(deftest readme-immutable
  (testing "IGraphImmutable"
    (when (= (igraph/mutability @eg) ::igraph/immutable)
      (is (= (satisfies? igraph/IGraphImmutable @eg)
             true))
      (is (= (igraph/normal-form 
              (igraph/add 
               @eg
               [[:ig-ctest/chicken :ig-ctest/subclass-of :ig-ctest/meat]
                [:ig-ctest/beef :ig-ctest/subclass-of :ig-ctest/meat]
                ]))
             {:ig-ctest/john {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/beef}},
              :ig-ctest/mary {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/chicken}},
              :ig-ctest/chicken {:ig-ctest/subclass-of #{:ig-ctest/meat}},
              :ig-ctest/beef {:ig-ctest/subclass-of #{:ig-ctest/meat}}}))
      (is (= (igraph/normal-form (igraph/subtract @eg [:ig-ctest/john]))
             {:ig-ctest/mary {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/chicken}}}))
      (is (= (igraph/normal-form (igraph/subtract
                                  @eg
                                  [:ig-ctest/john :ig-ctest/likes]))
             {:ig-ctest/john {:ig-ctest/isa #{:ig-ctest/person}}, 
              :ig-ctest/mary {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/chicken}}}))
      ))) ;; immutable


(def mutable-eg "Initialized with `eg-data`. Test for mutable graphs"
    (atom nil))

(deftest readme-mutable
  (testing "IGraphMutable"
    ;; requires a dedicated @mutable-eg
    (when @mutable-eg
      (is (= (igraph/mutability @mutable-eg)
             ::igraph/mutable))
      (is (= (satisfies? igraph/IGraphMutable @mutable-eg)
             true))
      (is (= (igraph/normal-form @mutable-eg)
             eg-data))
      (is (= (igraph/normal-form 
              (igraph/add! 
               @mutable-eg
               [[:ig-ctest/chicken :ig-ctest/subclass-of :ig-ctest/meat]
                [:ig-ctest/beef :ig-ctest/subclass-of :ig-ctest/meat]
                ]))
             {:ig-ctest/john
              {:ig-ctest/isa #{:ig-ctest/person},
               :ig-ctest/likes #{:ig-ctest/beef}},
              :ig-ctest/mary {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/chicken}},
              :ig-ctest/chicken {:ig-ctest/subclass-of #{:ig-ctest/meat}},
              :ig-ctest/beef {:ig-ctest/subclass-of #{:ig-ctest/meat}}}))
      (is (= (igraph/normal-form
              (igraph/subtract!
               @mutable-eg
               [[:ig-ctest/chicken :ig-ctest/subclass-of :ig-ctest/meat]
                [:ig-ctest/beef :ig-ctest/subclass-of :ig-ctest/meat]
                ]))
             eg-data))
      (is (= (igraph/normal-form
              (igraph/subtract! @mutable-eg [:ig-ctest/john]))
             {:ig-ctest/mary {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/chicken}}}))

    )))

(deftest readme-set-operations
  (testing "IGraphSet"
    (when (satisfies? igraph/IGraphSet @eg)
      (is (= (igraph/normal-form @other-eg) other-eg-data))
      (is (= (satisfies? igraph/IGraphSet @other-eg)
             true))
      (is (= (igraph/normal-form (igraph/union @eg @other-eg))
             {:ig-ctest/john {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/beef}},
              :ig-ctest/mary {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/pork :ig-ctest/chicken}},
              :ig-ctest/waldo {:ig-ctest/isa #{:ig-ctest/person},
                               :ig-ctest/likes #{:ig-ctest/beer}}}))
      (is (= (igraph/normal-form (igraph/intersection @eg @other-eg))
             {:ig-ctest/mary {:ig-ctest/isa #{:ig-ctest/person}}}))
      (is (= (igraph/normal-form (igraph/difference @eg @other-eg))
             {:ig-ctest/john {:ig-ctest/isa #{:ig-ctest/person},
                              :ig-ctest/likes #{:ig-ctest/beef}},
              :ig-ctest/mary {:ig-ctest/likes #{:ig-ctest/chicken}}}))
      (is (= (igraph/normal-form (igraph/difference @other-eg @eg))
             {:ig-ctest/mary {:ig-ctest/likes #{:ig-ctest/pork}},
              :ig-ctest/waldo {:ig-ctest/isa #{:ig-ctest/person},
                               :ig-ctest/likes #{:ig-ctest/beer}}}))))
  )


  
