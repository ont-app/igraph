(ns ^{:author "Eric D. Scott",
      :doc "Abstractions over a graph object, intended to sit alongside the 
other basic clojure data structures such as maps and sequences.
"}
    igraph.core
  (:require [clojure.set :as set]
            )
  (:gen-class))


(defprotocol IGraph
  "An abstraction for graphs"
  (normal-form [g] "Returns <map> s.t. #(keys %) -> #{::schema ::contents}
Where 
<schema> :=  (keys %) -> #{::indexed-as ...}
<contents> := a map s.t. (keys %) -> {<subject> {<predicate> #{<object>...}...}...}
<indexed-as> := [<functional-subject> <function-predicate> <functional-object>],
  an exhaustive and non-repeating vector of <canonical-args>.
  By default: [::subject ::predicate ::object]
<function-subject> is one of <canonical-args>
<functional-predicate> is one of <canonical-args>
<function-object> is one of <canonical-args>
<canonical-args> :=  #{::subject ::predicate ::object}
<subject> is a hashable value (typically a keyword) indentifying some functional
  subject
<predicate> is a hashable value (typically a keyword) identifying some function
  predicate
<object> is an arbitrary clojure object.
")
  (add [g to-add])
  (get-p-o [g s])
  (get-o [g s p])
  (ask [g s p o])
  ;; for IFn
  (invoke [g s] [g s p] [g s p o])
  )

(defn add-to-graph-dispatcher [g to-add]
  "Returns dispatcher for add method given <g> and <to-add>
This is one of #{:vector-of-triples :triple (type <to-add)}
  " 
  (if (= (type to-add) clojure.lang.PersistentVector)
    (if (= (type (to-add 0)) clojure.lang.PersistentVector)
      :vector-of-triples
      :triple)
    ;; else not a vector
    (type to-add)))

(defmulti add-to-graph add-to-graph-dispatcher)

(deftype Graph [schema contents]
  IGraph
  (normal-form [g]
    {:schema (.schema g)
     :contents (.contents g)
     })
  (add [g to-add] (add-to-graph g to-add))
  (get-p-o [g s] (get (.contents g) s))
  (get-o [g s p] (get-in (.contents g) [s p]))
  (ask [g s p o] (not (not (get-in (.contents g) [s p o]))))
  
  clojure.lang.IFn
  (invoke [g s] (get-p-o g s))
  (invoke [g s p] (get-o g s p))
  (invoke [g s p o] (ask g s p o)))



(defmethod add-to-graph :vector-of-triples [g triples]
  (let [collect-triple (fn [acc triple]
                         (def _triple triple)
                         (assert (= (count triple) 3))
                         (let [[s p o] triple
                               ]
                           (update-in acc
                                      [s p]
                                      #(conj (set %) o))))
        ]
    (def _vector-of-triples triples)
    (make-graph
     :schema (.schema g)
     :contents (reduce collect-triple (.contents g) triples))))

(defmethod add-to-graph :triple [g triple]
  (add-to-graph g [triple]))

(defmethod add-to-graph clojure.lang.LazySeq [g seq]
  (add-to-graph g (vec seq)))

(defmethod add-to-graph :default [g to-add]
  (throw (Exception. (str "nyi for" (type to-add)))))

(defn make-graph
  ([&{:keys [schema contents]
      :or {schema [::subject ::predicate ::object]
           contents {}}}]
   (Graph. schema contents)))

(def test-graph (let [g (make-graph)
                      ]
                  (.add g
                        [[:john :isa :person]
                         [:mary :isa :person]
                         [:likes :isa :property]
                         [:isa :isa :property]
                         [:john :likes :meat]
                         [:mary :likes :coke]
                         [:meat :isa :food]
                         [:meat :isa :killer]
                         [:coke :isa :drink]
                         [:coke :isa :killer]
                         [:mary :name {:value "mary" :lang "en"}]
                         [:john :name {:value "john" :lang "en"}]
                         ])))
  


;;; simple queries

                 
(defn query-var?
  "Returns true iff <spec> is a query variable (symbol whose name
  starts with ?)
  "
  [spec]
  (def _spec spec)
  (not (nil? (re-matches #"^\?.*" (name spec)))))

(defn -matches-spec?
  "
  Returns true if <target> matches <spec> in <query context>
  Where
  <query-context> := (keys %) -> subset of #{:var-tests ...} typically
    the 'context' argument in a X-matches function.
  <spec> := is an element in some query clause
  <target> is an element in some Graph being matched in some query.
  "
  {:test (fn [] (= (-matches-spec? {:var-tests {:?x (fn [x] false)}} :?x "blah")
                   false))
   }
  [query-context spec target]
  (let [var-tests (:var-tests query-context)
        ]
    (if (query-var? spec)
      (or (not var-tests)
          (or (not (spec var-tests))
              ((spec var-tests) target)))
      ;; else not a query var
      (= spec target))))

(defn -annotate-match
  [spec value match]
  (if (:matched? match)
    (if (query-var? spec)
      (merge match {spec value})
      ;; else not a var
      match)
    ;; else not matched
    match))


(defn -o-match 
  "
  Returns <match-result> 
  Where
  <match-result> := (keys %) -> #{:matched? &maybe <var>}
  <next-o> is a map s.t. (keys %) -> #{:spec :s :p :o}
  <matched?> is true iff (var? spec) and (-test-var context o next-o))
    or (= <o> <next-o>)
  <spec> is either a variable or an id
  <s> is an ID bound the effective subject
  <p> is an ID bound the effective predicate
  <var> is <spec>, iff (var? spec)
  "
  [g context next-o]
  (let [[o] (:spec context)
        ]
    (-annotate-match
     o
     next-o
     {:matched? (-matches-spec? context o next-o)})))
                                


(defn test-object-match
  []
  (let [g (.add (make-graph) [:a :b :c])
        ]
    (-o-match g {:s :a :p :b :spec [:c]} :c)))

(defn -p-o-matches
  "Returns <match-results> for <g> <context> and <next-p>
  Where
  <match-results> := [<match-result> ...]
  <match result> := (keys %) -> {:matched? <var>, ...}
  <matched?> is true iff (query-var? (:spec context))
  <g> is a Graph
  <context> := (keys %) -> {:s :spec}
  <s> is a subject in <g> and the <s> argument in some query triple.
  <spec> := [<p> <o>] derived from some query triple.
  "
  [g context next-p]
  (let [[p o] (:spec context)
        ]
    (if (not (-matches-spec? context p next-p))
      {:matched? false}
      ;; else it matches the spec
      (map (partial -annotate-match p next-p)
           (filter :matched?
                   (map (partial -o-match
                                 g
                                 (assoc context
                                        :p p
                                        :spec [o]))
                        (g (:s context) next-p)))))))

(defn test-p-o-matches []
  (let [g (.add (make-graph)
                [[:a :b :c]
                 [:a :b :d]])
        ]
    (-p-o-matches g {:s :a :spec [:?b :?c]} :b)))
  
  
(defn -s-p-o-matches
  "Returns <match-results> for <g> <context> and <next-s>
  Where
  <match-results> := [<match-result> ...]
  <match result> := (keys %) -> {:matched? <var>, ...}
  <matched?> is true iff (query-var? <s>) which meets constraints in <var-tests>
    or (= <s> <next-s>)
  <g> is a Graph
  <context> := (keys %) -> {:spec :var-tests}
  <spec> := [<s> <p> <o>], a query triple.
  <s> is a subject in <g> and the <s> argument in <spec>
  <var-tests> := {<var> <test> ...}
  <test> := (fn [next-s]) -> true if next-s cannot be excluded from matching
    in the current context.
  "
  [g context next-s]
  (let [[s p o] (:spec context)
        
        matches? (or (and (query-var? s)
                          (-test-var context s next-s))
                     (= (s next-s)))
        ]
    (if (not (-matches-spec? context s next-s))
      {:matched? false}
      ;; else it matches the spec
      (map (partial -annotate-match s next-s)
           (filter :matched?
                   (mapcat (partial -p-o-matches
                                    g
                                    (assoc context
                                           :s next-s
                                           :spec [p o]))
                           (keys (g next-s))))))))

(defn test-s-p-o-matches []
  (let [g (.add
           (make-graph)
           [[:a :b :c]
            [:a :b :d]])
        ]
    (-s-p-o-matches g {:spec [:?a :?b :?c]} :a)))

(defn -query-clause-matches
  "Returns <results> for <clause> posed against <g>
  Where
  <results> := [<result> ...]
  <result> := map , s.t. #(keys %) -> #{:matched? <var> <value> ....}
  <clause> :=[<s> <p> <o>], a line from a simple query
  <g> is an instance of Graph.
  <s> <p> and <o> are either variables or clojure values which must match
    elements of <g> exactly.
  variables are symbols s.t. (query-var? %) -> true (keywords starting with ?)
  <matched?> is true iff <clause> matches the contents of the graph.
  <var> names the subset of <s> <p> <o> for which (query-var? %) is true
  <value> is a single value in <g> matching <var>
  "
  ([g clause var-tests]
   (let [[s p o] clause
         ]
     (map #(dissoc % :matched?)
          (filter :matched?
                  (mapcat  (partial -s-p-o-matches g {:var-tests var-tests
                                                      :spec clause})
                           (if (query-var? s)
                             (keys (.contents g))
                             [s]))))))
  ([g clause]
   (-query-clause-matches g clause {})))

(defn test-query-clause-matches []
  (let [g (.add
           (make-graph)
           [[:a :b :c]
            [:a :b :d]])
        ]
    (-query-clause-matches g [:?a :?b :?c])))


(def test-query-1 {:find [:?liker :?likee]
                   :where [[:?liker :likes :?likee]]})

(def test-query-2 {:find [:?liker :?likee :?class]
                   :where [[:?liker :likes :?likee]
                           [:?likee :isa :?class]
                           ]})
                  

(defn -collect-clause-match
  "Returns <clause-state> modified for <matches> to <g> in <query-state>
  Where
  <clause-state> := (keys %) -> #{:bindings} appended by <match> applied to
    appropriate candidates in (:bindings <query-state>)
  <query-state> := (keys %) -> #{:bindings :specified :matched?},
    modified s.t. each match found for clause is joined with compatible
    existing matches.
  <bindings> := #{<binding>...} typically to some query clause
  <binding> a valid match integrating compatible matches form all previous
    clauses
  <specified> := {<var> {<value> <specified bindings> ...}...}
  <specified bindings> := #{<specified match>...} a subset of <matches> specified
    for <var> and <value>
  "
  [g query-state clause-state match]
  (def _query-state query-state)
  #dbg
  (assoc clause-state
         :bindings
         (set/union
          (or (:bindings clause-state) #{})
          (if (not (empty? (:shared-bound clause-state)))
            (set (map (partial merge match)
                      (reduce set/intersection
                              (map (fn [qvar]
                                     (-> query-state
                                         :specified
                                         (.get-o qvar (qvar match))))
                                   (:shared-bound clause-state)))))
                                              
            ;;else the bindings in this clause have no precedent
            (if (:bindings query-state)
              (set (map (partial merge match)
                        (:bindings query-state)))
              ;;else there are no bindings in the query state
              #{match})))))
                                 

(defn -triplify-binding [binding]
  "Returns [[<var> <value> <binding>]...] for <binding>
Where
<binding> := {<var> <value> ...}
<var> is a query-var
<value> is typically a value bound to <var> in some query match
Note: this is typically used to populate the 'specified' graph in 
  a query-state
  "
  [binding]
  (let [triplify-var (fn [binding qvar]
                       [qvar (qvar binding) binding])
        ]
    (vec (map (partial triplify-var binding)
              (filter query-var? (keys binding))))))

(defn test-collect-clause-match []
  (let [clause (-> test-query-1
                         :where
                         (nth 0))
        clause-matches (-query-clause-matches test-graph clause)
        match (nth clause-matches 0)
        previous-bindings #{{:?liker :john
                             :?likee :meat}
                            }
        ]
    (-collect-clause-match test-graph
                           {:bindings previous-bindings
                            :specified (.add (make-graph)
                                             (mapcat -triplify-binding
                                                     previous-bindings))
                            }
                           {:shared-bound #{:?liker :?likee}
                            :bindings #{}
                            }
                           match)))
  
(defn -collect-clause-matches
  "Returns <query-state> modified for matches to <clause> in <g>
  Where
  <g> is a Graph
  <query-state> := (keys %) -> #{:matches :specified :unspecified :valid?},
    modified s.t. each match found for clause is joined with compatible
    existing matches.
  <clause> is a triple of query specs, typically part of some query
  <matches> := #{<match>...} which match <clause> in <g>
  <match> a valid match integrating compatible matches form all previous
    clauses
  <specified> := {<var> {<value> <specified matches> ...}...}
  <specified matches> := #{<specified match>...} a subset of <matches> specified
    for <var> and <value>
  <unspecified> := {<var> <unspecified matches>...}
  <unspecified matches> := #{<unspecified match>...} a subset of <matches>
    unspecified for <var>
  "
  [g query-state next-clause]
  (let [
        initial-clause-state
        {
         :shared-bound (set/intersection
                        (or (and (:specified query-state)
                                 (set (-> query-state
                                          :specified
                                          normal-form
                                          :contents
                                          keys)))
                            #{})
                        (set (filter query-var? next-clause)))
         }
        
        ]
    (let [clause-state 
          (reduce (partial -collect-clause-match g query-state)
                  initial-clause-state
                  (-query-clause-matches g next-clause))
          ]
      (assoc query-state
             :bindings (:bindings clause-state)
             :specified (add (make-graph)
                             (mapcat -triplify-binding
                                     (:bindings clause-state)))))))
    

(defn test-collect-clause-matches-1 []
  #dbg
  (let [clauses (-> test-query-1
                         :where)
        ]
    (reduce (partial -collect-clause-matches test-graph)
            {}
            clauses)))


(defn test-collect-clause-matches-2 []
  #dbg
  (let [clauses (-> test-query-2
                         :where)
        ]
    (reduce (partial -collect-clause-matches test-graph)
            {}
            clauses)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (normal-form (make-graph)))
