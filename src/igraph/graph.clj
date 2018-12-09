(ns ^{:author "Eric D. Scott",
      :doc "Implementation of a simple graph type implementing IGraph.
On typically adds to it with [[<s> <p> <o>]...].
One queries it with a simple graph pattern of the form [[<s> <p> <o>]...]
With variables of the form :?x.
"}
    igraph.graph
  (:require [clojure.set :as set]
            [igraph.core :refer :all]
            )
  #_(:gen-class))


(defn alter-graph-dispatcher [g to-add-or-remove]
  "Returns one of #{:vector :vector-of-vectors <type>} for <args>
  Where
  <args> := [<g> <to-alter>],  arguments to a method add or remove from graph
  <g> is a graph
  <to-alter> is a specification of triples to add to or remove from  <g>
  <triple> indicates <to-alter> := [<s> <p> <o>]
  <vector-of-vectors> indicates <to-alter> := [<triple>...]
  <type> = (type <to-add>)
  "
  (if (= (type to-add-or-remove) clojure.lang.PersistentVector)
    (if (= (type (to-add-or-remove 0)) clojure.lang.PersistentVector)
      :vector-of-vectors
      :vector)
    ;; else not a vector
    (type to-add-or-remove)))

(defmulti add-to-graph
  "Returns <g>, with <to-add> added
  Where
  <g> is a Graph
  <to-add> is interpetable as a set of triples
  "
  alter-graph-dispatcher)

(defmulti remove-from-graph
  "Returns <g>, with <to-add> added
  Where
  <g> is a Graph
  <to-add> is interpetable as a set of triples
  "  
  alter-graph-dispatcher)


(declare query-graph) ;; defined below

(deftype Graph [schema contents]

  IGraph
  (normal-form [g] (.contents g))
  (subjects [g] (keys (.contents g)))
  (get-p-o [g s] (get (.contents g) s))
  (get-o [g s p] (get-in (.contents g) [s p]))
  (ask [g s p o] (get-in (.contents g) [s p o]))
  (query [g q] (query-graph g q))
  (read-only? [g] false)
  (add [g to-add] (add-to-graph g to-add))
  (subtract [g to-subtract] (remove-from-graph g to-subtract))
  
  clojure.lang.IFn
  (invoke [g] (normal-form g))
  (invoke [g s] (get-p-o g s))
  (invoke [g s p] (get-o g s p))
  (invoke [g s p o] (ask g s p o)))



(defn make-graph
  "Returns <graph>, intialized per optional <schema> and <contents>
  Where
  <schema> is not presently used
  <contents> is a normal-form representation of initial contents.
    see also igraph/normal-form.
  "
  ([&{:keys [schema contents]
      :or {schema [::subject ::predicate ::object] ;; TODO make this relevant
           contents {}}}]
   (Graph. schema contents)))

(defmethod add-to-graph :vector-of-vectors [g triples]
  "
Where <triples> := [<v> ....]
<v> := [<s> <p1> <o1> <p2> <o2> ...<pn> <on>] 
"
  (let [collect-triple (fn [s acc [p o]]
                         (update-in acc
                                    [s p]
                                    #(conj (set %) o)))                       
        collect-vector (fn [acc v]
                         (assert (odd? (count v)))
                         (reduce (partial collect-triple (first v))
                                 acc
                                 (partition 2 (rest v))))
        ]
    (make-graph
     :schema (.schema g)
     :contents (reduce collect-vector (.contents g) triples))))

(defmethod add-to-graph :vector [g triple]
  (assert (= (count triple) 3))
  (add-to-graph g [triple]))

(defmethod add-to-graph clojure.lang.LazySeq [g the-seq]
  (add-to-graph g (vec the-seq)))


(defn- -dissoc-in [map-or-set path]
  "removes the last key in <path> from its parent in <map-or-set>, removing
    any empty containers along the way.
Where 
<map-or-set> is typically a sub-tree of graph contents
<path> := [<key> ...]
Note: typically used to inform removal of nodes in a graph, where <key> is 
  a subject, predicate or object
"
  (let [key (first path)
        ]
    (assert (not (empty? path)))
    (if (= (count path) 1)
      (if (set? map-or-set)
        (disj map-or-set key)
        ;; else it's a map
        (dissoc map-or-set key))
      ;; else there's more path
      (let [dissociated (-dissoc-in (get map-or-set key)
                                    (rest path))
            ]
        (if (empty? dissociated)
          (dissoc  map-or-set key)
          (assoc map-or-set key
                 dissociated))))))

(defmethod remove-from-graph :vector-of-vectors [g triples]
  "
Where <triples> := [<v> ....]
<v> := [<s> <p1> <o1> <p2> <o2> ...<pn> <on>] , or [<s>] or [<s> <p>]
<s> is a subject in <g>
<p> is a predicate for <s> in <g>
<o> is an object for <s> and <p> in <g>
Note: 
<v> = [<s>] signals that all {<p> <o>} s.t. (<g> <s>) should be removed.
<v> = [<s> <p>] signals that all <o> s.t. (<g> <s> <p>) should be removed
"
  (let [remove-triple (fn [s acc [p o]]
                        (-dissoc-in acc [s p o]))
        collect-vector (fn [acc v]
                         (if (< (count v) 3) ;; specifies s or s-p
                           (-dissoc-in acc v)
                         ;; else this specifies one or more triples...
                         (let []
                           (assert (odd? (count v)))
                           (reduce (partial remove-triple (first v))
                                   acc
                                   (partition 2 (rest v))))))
        ]
    (make-graph
     :schema (.schema g)
     :contents (reduce collect-vector (.contents g) triples))))

(defmethod remove-from-graph :vector [g to-remove]
  "Where
<to-remove> may be [s] [s p] [s p o]
"
  (assert (<= (count to-remove) 3))
  (let [contents (-dissoc-in (.contents g) to-remove)
        ]
    (make-graph
     :schema (.schema g)
     :contents (-dissoc-in (.contents g)
                           to-remove))))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for simple queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 
(defn query-var?
  "Returns true iff <spec> is a query variable (symbol whose name
  starts with ?)
  "
  [spec]
  (not (nil? (re-matches #"^\?.*" (name spec)))))

(defn- -matches-spec?
  "
  Returns true if <target> matches <spec> in <query context>
  Where
  <target> is an element in some Graph being match-tested in some query.
  <spec> := is an element in some query clause
  <query-context> := (keys %) -> #{maybe :var-tests ...} , typically
    the 'context' argument in an -X-matches function.
  "
  {:test (fn [] (= (-matches-spec? {:var-tests {:?x (fn [x] false)}} :?x "blah")
                   false))
   }
  [query-context spec target]
  (let [var-tests (:var-tests query-context)
        ]
    (if (query-var? spec)
      (or (not var-tests)
          (or (not (spec var-tests)) ;; no test, so match var
              ((spec var-tests) target))) 
      ;; else not a query var 
      (= spec target))))

(defn- -annotate-match
  "Returns <match> modified per <spec> and <value> only if (query-var? <spec>)
  Where
  <match> := (keys %) -> #{:matched? <var> ...}, typically acquired matching
    a graph pattern
  <spec> is an element of a graph pattern.
  <var> is a variable spec which matched <value>
  <value> is a value to be asigned to <spec> if (query-var? <spec>)
  "
  [spec value match]
  (if (:matched? match)
    (if (query-var? spec)
      (merge match {spec value})
      ;; else not a var
      match)
    ;; else not matched
    match))

(defn- -o-match 
  "
  Returns <match-result> to <next-o> in <context> querying <g>
  Where
  <match-result> := (keys %) -> #{:matched? &maybe <var>}
  <g> is a graph>
  <context> := (keys %) -> #{:spec :s :p :o}
  <next-o> is a graph element to be matched against <o>
  <o> is a graph pattern element in <spec>
  <matched?> is true iff <o> matches <next-o> in <context>
  <spec> :- [<o>] the last pattern element in a query clause
  <s> and <p> are graph elements mathed to the first two pattern elements
  <var> is <spec>, iff (query-var? spec)
  "
  [g context next-o]
  (let [[o] (:spec context)
        ]
    (-annotate-match
     o
     next-o
     {:matched? (-matches-spec? context o next-o)})))
                                
(defn- -p-o-matches
  "Returns <match-results> for <g> in <context> given <next-p>
  Where
  <match-results> := [<match-result> ...]
  <match result> := (keys %) -> #{:matched? <var>, ...}
  <matched?> is true iff <p> matched <next-p> and <o> matched downstream
  <g> is a Graph
  <context> := (keys %) -> {:s :spec}
  <s> is a subject element of <g> and the <s> argument in some graph pattern
    clause
  <spec> := [<p> <o>] derived from some graph pattern clause.
  "
  [^Graph g
   ^clojure.lang.PersistentArrayMap context
   ^clojure.lang.PersistentVector next-p]
  (let [[p o] (:spec context)
        ]
    (if (not (-matches-spec? context p next-p))
      {:matched? false}
      ;; else it matches <p>...
      (map (partial -annotate-match p next-p)
           ;; try to match on <o> ...
           (filter :matched?
                   (map (partial -o-match
                                 g
                                 (assoc context
                                        :p p
                                        :spec [o]))
                        (g (:s context) next-p)))))))

(defn- -s-p-o-matches
  "Returns <match-results> for <g> <context> and <next-s>
  Where
  <match-results> := [<match-result> ...]
  <match result> := (keys %) -> #{:matched? <var>, ...}
  <matched?> is true iff <s> matched <next-s> and <p> and <o> matched downstream
  <g> is a Graph
  <context> := (keys %) -> #{:spec :var-tests}
  <spec> := [<s> <p> <o>], a query triple.
  <s> specifies a match to subjects in the graph pattern clause
  <var-tests> := {<var> <test> ...}
  <test> := (fn [next-s]) -> true if next-s cannot be excluded from matching
    in the current context.
  "
  [^Graph g
   ^clojure.lang.PersistentArrayMap context
   next-s]
  (let [[s p o] (:spec context)
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


(defn- -query-clause-matches
  "Returns <results> for <clause> posed against <g>
  Where
  <results> := [<result> ...]
  <result> := #(keys %) -> #{:matched? <var> ....}
  <clause> :=[<s> <p> <o>], a line from a simple query
  <g> is an instance of Graph.
  <s> <p> and <o> are either variables or graph elements to match in the graph
    pattern
  <matched?> is true iff <clause> matches the contents of the graph.
  <var> names the subset of <s> <p> <o> for which (query-var? %) is true
  <value> is a single value in <g> matching <var>
  "
  ([^Graph g
    ^clojure.lang.PersistentVector clause
    ^clojure.lang.PersistentArrayMap var-tests]
   (let [[s p o] clause
         ]
     (map #(dissoc % :matched?)
          (filter :matched?
                  (mapcat  (partial -s-p-o-matches g {:var-tests var-tests
                                                      :spec clause})
                           (if (query-var? s)
                             (keys (.contents g))
                             [s]))))))
  ;; no var-tests specified
  ([g clause]
   (-query-clause-matches g clause {})))


(defn- -collect-clause-match
  "Returns <clause-state> modified for <match> to <g> in <query-state>
  Where
  <clause-state> := (keys %) -> #{:bindings :shared-bound} appended by
    <match> applied to appropriate candidates in (:bindings <query-state>)
  <query-state> := (keys %) -> #{:bindings :specified :matched?},
    modified s.t. each match found for clause is joined with compatible
    existing matches.
  <bindings> := #{<binding>...} typically to some query clause
  <binding> a valid match integrating compatible matches form all previous
    clauses
  <shared-bound> := #{<var> ...}, s.t. <var> is bound in <specified>, and
    also present in the current graph pattern clause. This means new bindings
    must align with the values already specified upstream.
  <specified> := {<var> {<value> <specified bindings> ...}...}, a Graph
  <specified bindings> := #{<specified match>...} a subset of <matches> for which
    <var> was bound to <value> in previous clauses.
  "
  [^Graph g
   ^clojure.lang.PersistentArrayMap query-state
   ^clojure.lang.PersistentArrayMap clause-state
   ^clojure.lang.PersistentArrayMap match]
  (assoc clause-state
         :bindings
         (set/union
          (or (:bindings clause-state) #{})
          (if (not (empty? (:shared-bound clause-state)))
            (set (map (partial merge match)
                      (reduce set/intersection
                              (map (fn [qvar]
                                     (-> query-state
                                         :specified ;; TODO consider change :specified to :history or :established-bindings
                                         (.get-o qvar (qvar match))))
                                   (:shared-bound clause-state)))))
                                              
            ;;else the bindings in this clause have no precedent
            (if (:bindings query-state)
              (set (map (partial merge match)
                        (:bindings query-state)))
              ;;else there are no bindings in the query state
              #{match})))))
                                 

(defn- -triplify-binding [binding]
  "Returns [[<var> <value> <binding>]...] for <binding>
Where
<binding> := {<var> <value> ...}
<var> is a query-var
<value> is typically a value bound to <var> in some query match
Note: this is typically used to populate the 'specified' graph in 
  a query-state, which informs the matching process downstream.
  "
  [^clojure.lang.PersistentArrayMap binding]
  (let [triplify-var (fn [binding qvar]
                       [qvar (qvar binding) binding])
        ]
    (vec (map (partial triplify-var binding)
              (filter query-var? (keys binding))))))

(defn- -collect-clause-matches
  "Returns <query-state> modified for matches to <clause> in <g>
  Where
  <query-state> := (keys %) -> #{:matches :specified},
    modified s.t. each match found for clause is joined with compatible
    existing matches.
  <clause> is a triple of query specs, typically part of some query
  <g> is a Graph
  <matches> := #{<match>...} which match <clause> in <g>
  <match> a valid match integrating compatible matches from this and all previous
    clauses
  <specified> := {<var> {<value> <specified matches> ...}...}, a Graph
  <specified matches> := #{<specified match>...} a subset of <matches> specified
    for <var> and <value>
  "
  [^Graph g
   ^clojure.lang.PersistentArrayMap query-state
   ^clojure.lang.PersistentVector next-clause]
  
  (let [
        initial-clause-state
        {
         ;; Collect all the variables in this clause which are shared
         ;; with matches in previous clauses, they will need to jibe...
         :shared-bound (set/intersection
                        (or (and (:specified query-state)
                                 (set (-> query-state
                                          :specified
                                          normal-form
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
    

(defn query-graph
  "Returns #{<binding>...} for <graph-pattern> applied to <g>
  Where
  <g> is a Graph
  <graph-pattern> := [[<var-or-value> <var-or-value> <var-or-value>]...]
  <var-or-value> is in #{<var> <value>}
  <var> is a keyword whose name begins with '?'
  <value> is a value which must match an element of <g> exactly.
  <binding> := {<var> <matching-value>, ...}
  <matching-value> matches <var> within <graph-pattern> applied to <g>
  "
  [^Graph g
   ^clojure.lang.PersistentVector graph-pattern]
  
  (:bindings
   (reduce (partial -collect-clause-matches g)
           {}
           graph-pattern)))

;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;

(defn unique
  "Returns the single member of <coll>, or nil if <coll> is empty. Calls <if-plural> if there is more than one member (default is to throw an Exception).
  Where
  <coll> is a collection
  <if-plural> := (fn [coll] ...) -> <value>, default raises an error.
  Note: this can be used when you've called (G s p) and you're sure there is
    only one object.
  "
  ([coll on-plural]
   (if (not (empty? coll))
     (if (> (count coll) 1)
       (on-plural coll)
       (first coll))))
  ([coll]
   (unique coll (fn [coll] (throw (Exception. (str "Non-unique: " coll)))))))
       
