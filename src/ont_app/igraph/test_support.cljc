(ns ont-app.igraph.test-support
  "These are tests that should be applicable to implemenations of the IGraph protocols. Most examples are drawn from the README. After running tests, query-for-failures should be empty."
  {
   ;; this metadata can be used by downstream libraries that rely on RDF URIs...
   :vann/preferredNamespacePrefix
   "igraph-test" ;; matches :: declarations directly
   :vann/preferredNamespaceUri
   "http://rdf.naturallexicon.org/ont-app/igraph/igraph-test#"
   }
  (:require
   [ont-app.igraph.core :as igraph]
   [ont-app.igraph.graph :as native-normal]
   [clojure.set]
   ))

;; FUN WITH READER MACROS
(def cljc-LazySeq
  "LazySeq type in clj(s)"
  #?(:clj clojure.lang.LazySeq
     :cljs cljs.core/LazySeq))

(def cljc-format
  "The format function in clj(s)"
  #?(:clj clojure.core/format
     :cljs goog.string.format))

;; NO READER MACROS BELOW THIS POINT

(def subClassOf*
  "A traversal function. Transitive closure of subClassOf"
  (igraph/transitive-closure :igraph-test/subClassOf))

(defn sans-schema 
  "Returns a native-normal graph with the contents of `g` minus `schema-graph`
  Where
  - `g` is the test graph implementing IGraph and containing test content
  - `schema` is a properly configured graph devoid of test content, or nil
  NOTE: Some graph implementations will require that you declare a schema up-front, for example Datascript or Datomic. We'll need to extract this before we confirm that we're starting with valid test content.
  "
  [g schema-graph]
  (if (not schema-graph)
    g
    ;; else there's some schema configuration
    (igraph/difference (native-normal/make-graph
                        :contents (igraph/normal-form g))
                       (native-normal/make-graph
                        :contents (igraph/normal-form schema-graph)))))

(defn do-report! 
  "Side-effect: Modifies `report` in include `desc`. Returns @report
Where
- `report` is an atom containing the report graph
- `desc` is a normal-form description to be added to `report`"
  [report desc]
  (swap! report igraph/add desc)
  @report)

(defn do-assert-and-report!
  "Modifies `report` with test results of `test-name` with `comment` from comparing `observed` to `expected`. Returns @report
  Where
  - `report` is an atom containing the report graph
  - `test-name` is a KWI naming a test
  - `comment` is a string describing the test
  - `observed` is some value derived when running the test
  - `expected` is the expected value of running the test.
  NOTE: if observed=expected, `test-name` will be of type ::Passed, else ::Failed.
  "
  [report test-fn-name test-name comment observed expected]
  (if (= observed expected)
    (do-report! report
                [test-name
                 :rdf/type ::Passed
                 ::inTest test-fn-name
                 :rdfs/comment (str comment " as expected")])
    ;; else
    (do-report! report
                [test-name
                 :rdf/type ::Failed
                 ::inTest test-fn-name
                 :rdfs/comment (str comment " not as expected")
                 ::observed observed
                 ::expected expected])))

(defn report-invalid-test-graph
  "Returns `report`' if `test-graph` is not valid, otherwise nil.
  Where
  - `report` is a report graph
  - `test-graph` is a graph provided for a test
  - `test-fn-var` is a #'var naming the operative test
  ` `protocol` is the expected protocol for `test-graph`
  - `content-var` is the var naming the test content, eg `#'eg-data`
  - `schema-graph` (optional) is a graph initialized with any required schema, but otherwise free of content. Default is nil.
  NOTE: typical pattern is (or (report-invalid-test-graph ...) (<do-the-actual-tests>))
  "
  [report test-graph & {:keys [test-fn-var protocol content-var schema-graph]}]
  (assert (var? content-var))
  (let [report' (atom report)
        ]
    (if (not test-graph)
      (do-report! report'
                  [::TestGraphExistsTest
                   :rdf/type ::Failed
                   ::inTest test-fn-var
                   :rdfs/comment (cljc-format "Test graph for %s was nil in %s"
                                              content-var
                                              test-fn-var
                                              )
                   ])
      ;; else the test graph exists
      (if #?(:clj (and protocol (not (satisfies? protocol test-graph)))
             :cljs false) ;; cljs doesn't do this well
        (do-report! report'
                    [::TestGraphSatisfiesProtocolTest
                     :rdf/type ::Failed
                     ::inTest test-fn-var
                     :rdfs/comment (cljc-format "Test graph for %s does not satisfy %s in %s"
                                           content-var
                                           protocol
                                           test-fn-var
                                           )
                     
                     ])
        ;; else the graph satisfies. How's the content?
        (if (not (= (igraph/normal-form (sans-schema test-graph schema-graph))
                    (deref content-var)))
          (do-report! report'
                      [::TestGraphContentTest
                       :rdf/type ::Failed
                       ::inTest test-fn-var
                       :rdfs/comment (cljc-format "Test graph for %s does not have expected contents in %s"
                                             content-var
                                             test-fn-var)
                       ::observed (igraph/normal-form (sans-schema test-graph schema-graph))
                       ::expected (deref content-var)
                       ]))))))


(def eg-data "Initial data for the `eg` graph in the README"
  {:igraph-test/john
   {:igraph-test/isa #{:igraph-test/person}, :igraph-test/likes #{:igraph-test/beef}},
   :igraph-test/mary
   {:igraph-test/isa #{:igraph-test/person}, :igraph-test/likes #{:igraph-test/chicken}}})

(def ^:private the igraph/unique)

(defn test-readme-eg-access
  "Returns `report'` for `report`, given `eg-graph`, possibly informed by `readme-schema-graph`
  Where
  -  `report` is a native-normal IGraph recording tests and their outcomes
  - `eg-graph` is a graph in the target IGraph implementation containing
    `readme-example-content`, created per the configuration of `report`
  - `readme-schema-graph` (optional) is a target IGraph implementation initialized with
     whatever schema configuration is required by the target implementation.
     (Datascript for example). Default is nil.
  NOTE: these tests are for the access functions defined for IGraph.
  "
  ([report]
   (let [make-graph (the (report ::StandardIGraphImplementationReport ::makeGraphFn))
         eg-graph (make-graph eg-data)
         schema-graph (the (report ::StandardIGraphImplementationReport ::schemaGraph))
         test-fn-var #'test-readme-eg-access
         report' (atom report)
         assert-and-report! (partial do-assert-and-report! report' test-fn-var)
         ]
     (or
      (report-invalid-test-graph report'
                                 eg-graph
                                 :test-fn-var test-fn-var
                                 :protocol igraph/IGraph
                                 :content-var #'eg-data
                                 :schema-graph schema-graph
                                 )
       (let []
         (assert-and-report!
          ::SansSchemaTest
          "eg-graph content after removing any schema"
          (igraph/normal-form (sans-schema eg-graph
                                           schema-graph))
          eg-data)
         (assert-and-report!
          ::SubjectsTest
          "Subjects of eg-graph should be john and mary"
          (if schema-graph
            (clojure.set/difference ;; remove subjects in schema graph if exists
             (set (igraph/subjects eg-graph))
             (set (igraph/subjects schema-graph)))
            (set (igraph/subjects eg-graph)))
          #{:igraph-test/john :igraph-test/mary})

         ;; the rest of these tests aren't affected by the schema graph
         (assert-and-report!
          ::LazySeqSubjectsTest
          "Type of (subjects eg-graph-data)"
          (type (igraph/subjects eg-graph))
          cljc-LazySeq)
         
         (assert-and-report!
          ::GetPOTest
          "(get-p-o eg-graph :igraph-test/john) output"
          (igraph/get-p-o eg-graph :igraph-test/john)
          {:igraph-test/isa #{:igraph-test/person},
           :igraph-test/likes #{:igraph-test/beef}})

         (assert-and-report!
          ::GetOTest
          "(igraph/get-o eg-graph :igraph-test/john :igraph-test/isa) output"
          (igraph/get-o eg-graph :igraph-test/john :igraph-test/isa)
          #{:igraph-test/person})
         
         (assert-and-report!
          ::AskTruthyTest
          "(ask eg-graph john likes beef) output"
          (some? (igraph/ask eg-graph
                             :igraph-test/john
                             :igraph-test/likes
                             :igraph-test/beef))
          true)
         
         (assert-and-report!
          ::AskFalseyTest
          "(igraph/ask eg-graph :john :likes :chicken)) output"
          (not
           (igraph/ask eg-graph
                       :igraph-test/john
                       :igraph-test/likes
                       :igraph-test/chicken))
          true)
         
         (assert-and-report!
          ::Invoke1ArgTest
          "(eg-graph :igraph-test/john) output"
          (eg-graph :igraph-test/john)
          {:igraph-test/isa #{:igraph-test/person},
           :igraph-test/likes #{:igraph-test/beef}})
         
         (assert-and-report!
          ::Invoke2ArgTest
          "(eg-graph :igraph-test/mary :igraph-test/likes) output"
          (eg-graph :igraph-test/mary :igraph-test/likes)
          #{:igraph-test/chicken})
         
         (assert-and-report!
          ::Invoke3ArgTestTruthy
          "(some? (eg-graph :mary :likes :chicken)) output"
          (some? (eg-graph :igraph-test/mary
                           :igraph-test/likes
                           :igraph-test/chicken))
          true)
         
         (assert-and-report!
          ::Invoke3ArgTestFalsey
          "(not (eg-graph :mary :likes :beef)) output"
          (some? (eg-graph :igraph-test/mary
                           :igraph-test/likes
                           :igraph-test/beef))
          false)
         
         ;; MUTABILITY
         (assert-and-report!
          ::MutabilityTestMembership
          "(igraph/mutability eg-graph) output"
          (some? (#{::igraph/read-only
                    ::igraph/immutable
                    ::igraph/mutable
                    ::igraph/accumulate-only
                    }
                  (igraph/mutability eg-graph)))
          true)
         
         (case (igraph/mutability eg-graph)
           
           ::igraph/immutable
           (assert-and-report!
            ::IGraphImmutableTest
            "Whether eg-graph satisfies IGraphMutable"
            (satisfies? igraph/IGraphImmutable eg-graph)
            true)
           
           ::igraph/mutable
           (assert-and-report!
            ::IGraphMutableTest
            "Whether eg-graph satisfies IGraphAccumuOnly"
            (satisfies? igraph/IGraphMutable eg-graph)
            true)
           
           ::igraph/accumulate-only
           (assert-and-report!
            ::IGraphAccumulateOnlyTest
            "Whether eg-graph satisfies IGraphAccumulateOnly"
            (satisfies? igraph/IGraphAccumulateOnly eg-graph)
            true)
           
           ::igraph/read-only nil
           
           ) ;; mutability cases

         ;; UTILITIES
         (letfn [(tally-triples [tally _s _p _o]
                   (inc tally))
                 ]
           (assert-and-report!
            ::TallyTriplesTest
            "Tally-triples applied as a reduce-spo should count triples properly"
            (- (igraph/reduce-spo tally-triples 0 eg-graph)
               (if schema-graph
                 (igraph/reduce-spo tally-triples 0 schema-graph)
                 0))
            4))
         )) ;; cond
     @report')))

(def types-data
  "Contents of the `eg-with-types` graph from the README."
  {
   :igraph-test/beef {:igraph-test/subClassOf #{:igraph-test/meat}},
   :igraph-test/chicken {:igraph-test/subClassOf #{:igraph-test/meat}}
   :igraph-test/pork {:igraph-test/subClassOf #{:igraph-test/meat}},
   :igraph-test/meat {:igraph-test/subClassOf #{:igraph-test/food}},
   :igraph-test/beer {:igraph-test/subClassOf #{:igraph-test/beverage}},
   :igraph-test/food {:igraph-test/subClassOf #{:igraph-test/consumable}},
   :igraph-test/beverage {:igraph-test/subClassOf #{:igraph-test/consumable}},
   :igraph-test/consumable {:igraph-test/subClassOf #{:igraph-test/thing}},
   :igraph-test/person {:igraph-test/subClassOf #{:igraph-test/thing}},
   })

(def eg-with-types-data
  "Data to populate a test graph with the `eg-with-types` example graph in README"
  (-> (native-normal/make-graph)
      (igraph/add eg-data)
      (igraph/add types-data)
      (igraph/normal-form)))

(defn test-readme-eg-traversal
  "Returns `report'` given `eg-with-types-graph`
  where
  - `report` is a native-normal graph
  - `eg-with-types-graph` is an instance of the target graph initialized with `eg-data` + `eg-with-types-data`, created per the configuration of `report`.
  NOTE: these tests all have to do with traversal.
  "
  ([report]
   (let [make-graph (the (report ::StandardIGraphImplementationReport ::makeGraphFn))
         eg-with-types-graph (make-graph eg-with-types-data)
         schema-graph (the (report ::StandardIGraphImplementationReport ::schemaGraph))
         test-fn-var #'test-readme-eg-traversal
         report' (atom report)
         assert-and-report! (partial do-assert-and-report! report' test-fn-var)
         ]
     (or
      (report-invalid-test-graph report'
                                 eg-with-types-graph
                                 :test-fn-var test-fn-var
                                 :protocol igraph/IGraph
                                 :content-var #'eg-with-types-data
                                 :schema-graph schema-graph
                                 )
      
      (let []
        (assert-and-report!
       ::EgWithTypesGraphContentsTest
       "Whether contents of eg-with-types-graph argument to test-readme-eg-traversal are as expected"
       (igraph/normal-form (sans-schema eg-with-types-graph schema-graph))
       eg-with-types-data
       )
      
      (assert-and-report!
       ::subClassOf*MeatTest
       "Checking output of subClassOf* against [meat]"
       (subClassOf* eg-with-types-graph {} #{} [:igraph-test/meat])
       [{} #{:igraph-test/meat} '(:igraph-test/food)])

      (assert-and-report!
       ::subClassOf*Meat+FoodTest
       "Checking output of subClassOf* against [food] (with meat in accumulator)"
       (subClassOf* eg-with-types-graph {} #{:igraph-test/meat} '(:igraph-test/food))
       [{} #{:igraph-test/meat :igraph-test/food} '(:igraph-test/consumable)])

      (assert-and-report!
       ::TraverseLinkIsaJohnMaryTest
       "traversing isa with john and mary -> person"
       (igraph/traverse eg-with-types-graph
                        (igraph/traverse-link :igraph-test/isa) 
                        #{}
                        [:igraph-test/john :igraph-test/mary])
       #{:igraph-test/person})

      (assert-and-report!
       ::MaybeTraverseLinkTest
       "Maybe-traverse `isa` from john & mary should include john and mary"
       (igraph/traverse eg-with-types-graph
                        (igraph/maybe-traverse-link :igraph-test/isa) 
                        #{} 
                        [:igraph-test/john :igraph-test/mary])
       #{:igraph-test/person :igraph-test/john :igraph-test/mary})

      (let [subsumed-by (igraph/traverse-or :igraph-test/isa :igraph-test/subClassOf)]
        
        (assert-and-report!
         ::SubsumedByTestIsa
         "subsumed-by should merge isa and subClassOf. john isa person."
         (igraph/traverse eg-with-types-graph subsumed-by #{} [:igraph-test/john])
         #{:igraph-test/person})
        
        (assert-and-report!
         ::SubsumedByTestSubClass
         "Subsumed-by with meat should return its superclass food"
         (igraph/traverse eg-with-types-graph subsumed-by #{} [:igraph-test/meat])
         #{:igraph-test/food}
         )
        ) ;; let subsumed-by

      (let [instance-of (igraph/t-comp
                         [:igraph-test/isa
                          (igraph/transitive-closure :igraph-test/subClassOf)])
            ]
        (assert-and-report!
         ::InstanceOfTest
         "instance-of = isa/subclassOf*"
         (igraph/traverse eg-with-types-graph instance-of #{} [:igraph-test/john])
         #{:igraph-test/person :igraph-test/thing})) ;; let instance-of

      (assert-and-report!
       ::SubClassOf*AsPropertyWithOpenObjectTest
       "Using subClassOf* as a property arg with open object should deliver all superclasses of beef"
       (eg-with-types-graph :igraph-test/beef subClassOf*)
       #{:igraph-test/consumable :igraph-test/beef
               :igraph-test/meat
         :igraph-test/food :igraph-test/thing})
      
      (assert-and-report!
       ::SubClassOf*AsPropertyWithSpecifiedObjectTestTruthy
       "Using subClassOf* as a property arg with specified object should be truthy when true"
       (eg-with-types-graph :igraph-test/beef subClassOf* :igraph-test/food)
       :igraph-test/food)

      (assert-and-report!
       ::SubClassOf*AsPropertyWithSpecifiedObjectTestFalsey
       "Using subClassOf* as a property arg with specified object should be falsey when false"
       (eg-with-types-graph :igraph-test/beef subClassOf* :igraph-test/person)
       nil)
      
      (assert-and-report!
       ::DirectCallToTCompTest
       "A direct call to t-comp in open property position"
       (eg-with-types-graph :igraph-test/john (igraph/t-comp
                                            [:igraph-test/likes subClassOf*]))
       #{:igraph-test/consumable
               :igraph-test/beef :igraph-test/meat :igraph-test/food :igraph-test/thing})

      (assert-and-report!
       ::Issue4Test
       "Default accumulator is []"
       (type (igraph/traverse eg-with-types-graph
                              (igraph/traverse-link :igraph-test/isa) 
                              [:igraph-test/john :igraph-test/mary]))
       (type []))
      )) ;;cond
    )))

(def cardinality-1-graph-data
  "Data to be loaded into the test graph for cardinality-1 examples in README"
  (-> (native-normal/make-graph :contents eg-data)
      (igraph/add
       {:igraph-test/john {:igraph-test/likes #{:igraph-test/beer}
                           :igraph-test/has-vector #{[1 2 3]}}})
      (igraph/normal-form)))

(defn test-cardinality-1
  "Returns `report'` using a graph containing data from the README
  Where
  - `report` is a report graph
  - `cardinality-1-graph` contains `cardinality-1-graph-data` plus maybe the contents
    of `schema-graph`
  - `schema-graph` is nil, or contains initialization data required by your igraph
    implementation.
  NOTE: these tests all relate to cardinality-1 behavior as documented in the README.
  "
  ([report]
   (let [make-graph (the (report ::StandardIGraphImplementationReport ::makeGraphFn))
         cardinality-1-graph (make-graph cardinality-1-graph-data)
         schema-graph (the (report ::StandardIGraphImplementationReport ::schemaGraph))
         test-fn-var #'test-cardinality-1
         report' (atom report)
         assert-and-report! (partial do-assert-and-report! report' test-fn-var)
         ]
     (or (report-invalid-test-graph report'
                                    cardinality-1-graph
                                    :test-fn-var test-fn-var
                                    :protocol igraph/IGraph
                                    :content-var #'cardinality-1-graph-data
                                    :schema-graph schema-graph)
         ;; else the graph is as expected.
         (let []
           (assert-and-report!
            ::UniqueTest
            "Call to unique on a singleton should return its only member"
            (igraph/unique (cardinality-1-graph :igraph-test/john :igraph-test/isa))
            :igraph-test/person)

           (assert-and-report!
            ::UniqueNonUniqueTest
            "Providing an `on-non-unique` argument as 'first'"
            ;; Using set as a function
            (-> (#{:igraph-test/consumable
                   :igraph-test/beef
                   :igraph-test/meat
                   :igraph-test/food
                   :igraph-test/thing}
                 (igraph/unique (cardinality-1-graph :igraph-test/beef subClassOf*)
                                first))
                some?)
            true)
           
           (assert-and-report!
            ::FlattenDescriptionTest
            "Flatten-description on a p-o should be a map with flattened singletons."
            (-> (igraph/flatten-description (cardinality-1-graph :igraph-test/john))
                :igraph-test/isa)
            :igraph-test/person)

           (assert-and-report!
            ::FlattenDescriptionWithVectorTest
            "Flattening a description with a vector object should preserve the vector."
            (igraph/flatten-description (cardinality-1-graph :igraph-test/john))
            {:igraph-test/isa :igraph-test/person,
             :igraph-test/likes #{:igraph-test/beef :igraph-test/beer},
             :igraph-test/has-vector [1 2 3]})
           
           (assert-and-report!
            ::NormalizeFlatDescriptionTest
            "Normalizing a flat description with a vector should restore normal form"
            (igraph/normalize-flat-description 
             {:igraph-test/isa :igraph-test/person,
              :igraph-test/likes #{:igraph-test/beef :igraph-test/beer},
              :igraph-test/has-vector [1 2 3]})
            {:igraph-test/isa #{:igraph-test/person},
             :igraph-test/likes #{:igraph-test/beef :igraph-test/beer},
             :igraph-test/has-vector #{[1 2 3]}})
           ));; cond
     )))

(defn test-readme-eg-mutation-fn
  "Returns `report'`, given `eg-graph` and maybe `schema-graph` if needed, informed by `context`
  Where
  - `report` is a graph containing test results
  - `eg-graph` contains `eg-data`, drawn from examples in the README.
  - `schema-graph` is nil, or contains initialization data required by your igraph
    implementation.
  - `context` := m s.t. (keys m) :~ #{:test-fn-var :protocol :add-fn :subtract-fn :assert-unique-fn}
  - `test-fn-var` is a var naming the test function for the :inTest attr in the report
  - `protocol` is the mutability protocol implemented in `eg-graph`
  - `add-fn` is a function appropriate to `protocol` for adding to `eg-graph`
  - `subtract-fn` is a function appropriate to `protocol` for removing from `eg-graph`.
  - `assert-unique-fn` is a function appropriate to `protocol` for asserting a triple whose object is a singleton (should clobber any previous object).
  "
  ([context report]
   (let [{:keys [test-fn-var protocol add-fn subtract-fn assert-unique-fn]} context
         make-graph (the (report ::StandardIGraphImplementationReport ::makeGraphFn))
         schema-graph (the (report ::StandardIGraphImplementationReport ::schemaGraph))
         report' (atom report)
         assert-and-report! (partial do-assert-and-report! report' test-fn-var)
         ]
     (or (report-invalid-test-graph report' (make-graph eg-data)
                                    :test-fn-var test-fn-var
                                    :protocol protocol
                                    :content-var #'eg-data
                                    :schema-graph schema-graph)
         
         ;; else eg-graph exists and satisfies
         (let []
           (let [eg-graph (make-graph eg-data)]
             (assert-and-report!
              ::AddMeatTest
              "Contents after adding meat subclasses"
              (igraph/normal-form
               (add-fn
                eg-graph
                [[:igraph-test/chicken :igraph-test/subclass-of :igraph-test/meat]
                 [:igraph-test/beef :igraph-test/subclass-of :igraph-test/meat]]))
              ,
              {:igraph-test/john {:igraph-test/isa #{:igraph-test/person},
                                  :igraph-test/likes #{:igraph-test/beef}},
               :igraph-test/mary {:igraph-test/isa #{:igraph-test/person},
                                  :igraph-test/likes #{:igraph-test/chicken}},
               :igraph-test/chicken {:igraph-test/subclass-of #{:igraph-test/meat}},
               :igraph-test/beef {:igraph-test/subclass-of #{:igraph-test/meat}}}
              ))
           
           (let [eg-graph (make-graph eg-data)]
             (assert-and-report!
              ::SubtractJohnTest
              "Contents after subtracting :john"
              (igraph/normal-form (subtract-fn eg-graph [:igraph-test/john]))
              ,
              {:igraph-test/mary {:igraph-test/isa #{:igraph-test/person},
                                  :igraph-test/likes #{:igraph-test/chicken}}}
              
              ))
           
           (let [eg-graph (make-graph eg-data)]
             (assert-and-report!
              ::SubtractJohnLikesTest
              "Contents afer subtracting :john :likes"
              (igraph/normal-form (subtract-fn
                                   eg-graph
                                   [:igraph-test/john :igraph-test/likes]))
              ,
              {:igraph-test/john {:igraph-test/isa #{:igraph-test/person}}, 
               :igraph-test/mary {:igraph-test/isa #{:igraph-test/person},
                                  :igraph-test/likes #{:igraph-test/chicken}}}))

           (let [eg-graph (make-graph eg-data)
                 g (assert-unique-fn eg-graph
                                     :igraph-test/john :igraph-test/isa :igraph-test/man)
                 ]
             (assert-and-report!
              ::AssertUniqueTest
              "Contents after assert-unique :john :isa :man"
              (g :igraph-test/john)
              ,
              {:igraph-test/likes #{:igraph-test/beef},
               :igraph-test/isa #{:igraph-test/man}}))

           ;; testing add/subtract for long vector
           (let [eg-graph (make-graph eg-data)
                 long-vector [:igraph-test/moe
                              :igraph-test/isa :igraph-test/person
                              :igraph-test/likes :igraph-test/meat]
                 g-added (add-fn eg-graph long-vector)
                 ]
             (assert-and-report!
              ::AddLongvVectorTest
              "Adding vector with 5 elements"
              (g-added :igraph-test/moe)
              {:igraph-test/isa #{:igraph-test/person}
               :igraph-test/likes #{:igraph-test/meat}})
             (let [g-subtracted (subtract-fn g-added long-vector)]
               (assert-and-report!
                ::SubtractLongvVectorTest
                "Subtracting vector with 5 elements"
                (g-subtracted :igraph-test/moe)
                nil)))
           )))))


(defn test-readme-eg-mutation-dispatch
  "Returns the mutability of a graph made according to the report's ::makeGraphFn"
  [report]
  (let [make-graph (the (report ::StandardIGraphImplementationReport ::makeGraphFn))
        ]
    (-> eg-data
        (make-graph)
        (igraph/mutability))
  ))

(defmulti test-readme-eg-mutation
  "Returns `report`', modified per tests on mutability, dispatched on the mutability attribute of the graph under examination."
  test-readme-eg-mutation-dispatch)

(defmethod test-readme-eg-mutation ::igraph/immutable
  ([report]
   (test-readme-eg-mutation-fn
    {:protocol igraph/IGraphImmutable
     :add-fn igraph/add
     :subtract-fn igraph/subtract
     :assert-unique-fn igraph/assert-unique
     :test-fn-var #'test-readme-eg-mutation
     }
    report
    )))
  
(defmethod test-readme-eg-mutation ::igraph/mutable
  ;; "Returns report for mutations under a mutable igraph"
  ([report]
   (test-readme-eg-mutation-fn
    {:protocol igraph/IGraphImmutable
     :add-fn igraph/add
     :subtract-fn igraph/subtract
     :assert-unique-fn igraph/assert-unique
     :test-fn-var #'test-readme-eg-mutation
     }
    report
    )))

(defmethod test-readme-eg-mutation  ::igraph/accumulate-only 
  ;; "Returns report for mutations under an accumulate-only igraph"
  ([report]
   (test-readme-eg-mutation-fn
    {:protocol igraph/IGraphAccumulateOnly
     :add-fn igraph/claim
     :subtract-fn igraph/retract
     :assert-unique-fn igraph/claim-unique
     :test-fn-var #'test-readme-eg-mutation
     }
    report
    )))

(defmethod test-readme-eg-mutation  ::igraph/read-only
  ;; "Returns report for mutations under an accumulate-only igraph"
  ([report]
   ;; Nothing to mutate here
   report))



(def other-eg-data "Contents of the `other-eg` graph in the README"
  {:igraph-test/mary
   {:igraph-test/isa #{:igraph-test/person}, :igraph-test/likes #{:igraph-test/pork}},
   :igraph-test/waldo
   {:igraph-test/isa #{:igraph-test/person}, :igraph-test/likes #{:igraph-test/beer}}})

(defn test-readme-eg-set-operations
  "Returns `report'`, given `eg-graph` based on README examples
  Where
  - `report` is a graph containing test results
  - `eg-graph` implements IGraphSet and contains `eg-data`, drawn from examples in the README.
  - `other-graph` implements IGraphSet and contains `other-eg-data`, drawn from examples in the README.
  "
  ([report]
   (let [make-graph (the (report ::StandardIGraphImplementationReport ::makeGraphFn))
         eg-graph (make-graph eg-data)
         other-graph (make-graph other-eg-data)
         schema-graph (the (report ::StandardIGraphImplementationReport ::schemaGraph))
         test-fn-var #'test-readme-eg-set-operations
         report' (atom report)
         assert-and-report! (partial do-assert-and-report! report' test-fn-var)
         ]
     (or
      (report-invalid-test-graph report' eg-graph
                                 :test-fn-var test-fn-var
                                 :protocol igraph/IGraphSet
                                 :content-var #'eg-data
                                 :schema-graph schema-graph)
      
      (report-invalid-test-graph report' other-graph
                                 :test-fn-var test-fn-var
                                 :protocol igraph/IGraphSet
                                 :content-var #'other-eg-data
                                 :schema-graph schema-graph)
      
      ;; else both of our graphs are valid for this test...
      (let []
        (assert-and-report!
         ::UnionTest
         "Contents of union of `eg-graph` and `other-graph`"
         (igraph/normal-form (igraph/union eg-graph other-graph))
         ,
         {:igraph-test/john {:igraph-test/isa #{:igraph-test/person},
                             :igraph-test/likes #{:igraph-test/beef}},
          :igraph-test/mary {:igraph-test/isa #{:igraph-test/person},
                             :igraph-test/likes #{:igraph-test/pork :igraph-test/chicken}},
          :igraph-test/waldo {:igraph-test/isa #{:igraph-test/person},
                              :igraph-test/likes #{:igraph-test/beer}}})

        (assert-and-report!
         ::IntersectionTest
         "Contents of intersection of `eg-graph` and `other-graph`"
         (igraph/normal-form (igraph/intersection eg-graph other-graph))
         {:igraph-test/mary {:igraph-test/isa #{:igraph-test/person}}})

        (assert-and-report!
         ::DifferenceComingTest
         "Contents of `eg-graph` minus `other-graph`"
         (igraph/normal-form (igraph/difference eg-graph other-graph))
         {:igraph-test/john {:igraph-test/isa #{:igraph-test/person},
                             :igraph-test/likes #{:igraph-test/beef}},
          :igraph-test/mary {:igraph-test/likes #{:igraph-test/chicken}}})

        (assert-and-report!
         ::DifferenceGoingTest
         "Contents of `other-graph` minus `eg-graph`"
         (igraph/normal-form (igraph/difference other-graph eg-graph))
         {:igraph-test/mary {:igraph-test/likes #{:igraph-test/pork}},
          :igraph-test/waldo {:igraph-test/isa #{:igraph-test/person},
                              :igraph-test/likes #{:igraph-test/beer}}})
        )))))

(defn run-standard-implementation-tests
  "One-liner to test a fully-featured implemenation of all the IGraph protocols."
  [report]
  (-> report
      (test-readme-eg-access)
      (test-readme-eg-mutation)
      (test-readme-eg-set-operations)
      (test-readme-eg-traversal)
      (test-cardinality-1)
      )
  )

(defn query-for-failures
  "Returns #{`failure-binding`,...} for `report`
  Where
  - `failure-binding` := `m` s.t. (keys m) :~ #{:?test :?comment :?observed :?expected}
  - `report` is a graph put out by one or more tests in `test-support`
  - `:?test` names a test
  - `:?comment` is a string
  - `:?observed` is the value acquired in the test
  - `:?expected` is the expected value of `:?observed`
  - NOTE: a passing report should return an empty set.
  "
  [report]
  (let [maybe-assoc (fn [b k]
                      ;; add a value for `k` to `b` if observed
                      ;; where `b` is a binding for an error query with :?test
                      (if-let [b' (igraph/unique (igraph/query report
                                                               [[(:?test b) k :?value]]))
                               ]
                        (assoc b k (:?value b'))
                        ;; else
                        b))
        
        annotate (fn [sacc b]
                    ;; adds annotations to recognized errors
                   (conj sacc (-> b
                                  (maybe-assoc ::inTest)
                                  (maybe-assoc ::expected)
                                  (maybe-assoc ::observed)
                                  )))
        ]
    (reduce annotate
            #{}
            (native-normal/query-graph report
                                       [[:?test :rdf/type ::Failed]
                                        [:?test :rdfs/comment :?comment]
                                        ]))))

