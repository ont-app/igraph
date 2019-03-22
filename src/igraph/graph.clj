(ns ^{:author "Eric D. Scott",
      :doc "Implementation of a simple graph type implementing IGraph.
On typically adds to it with [[<s> <p> <o>]...].
One queries it with a simple graph pattern of the form [[<s> <p> <o>]...]
With variables of the form :?x.

The core type declaration:

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
  (invoke [g s p] (match-or-traverse g s p))
  (invoke [g s p o] (match-or-traverse g s p o))
  
  ISet
  (union [g1 g2] (add-to-graph g1 (g2)))
  (intersection [g1 g2] (get-intersection g1 g2))
  (difference [g1 g2] (remove-from-graph g1 (g2)))
  )

"}
    igraph.graph
  (:require [clojure.set :as set]
            [igraph.core :refer :all]
            )
  #_(:gen-class))


(declare query-graph) ;; defined below
(declare get-intersection)


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
  (invoke [g s p] (match-or-traverse g s p))
  (invoke [g s p o] (match-or-traverse g s p o))
  
  ISet
  (union [g1 g2] (add-to-graph g1 (g2)))
  (intersection [g1 g2] (get-intersection g1 g2))
  (difference [g1 g2] (remove-from-graph g1 (g2)))
  )


(defn make-graph
  "Returns <graph>, intialized per optional <schema> and <contents>
  Where
  <graph> is an instance of the `Graph` record, which implments `IGraph`, `Ifn` and `ISet`
  <schema> is not presently used
  <contents> is a normal-form representation of initial contents.
    see also igraph/normal-form.
  "
  ([&{:keys [schema contents]
      :or {schema [::subject ::predicate ::object] ;; TODO make this relevant
           contents {}}}]
   {:pre [(= (triples-format contents) :normal-form)]
    }
   (Graph. schema (with-meta contents {:triples-format :normal-form}))))


(defn vector-of-triples [g]
  "Returns (g) as #{[<s> <p> <o>]...}"
  (letfn [(collect-o [s p triples o]
            (conj triples [s p o])
            )
          (collect-p-o [s triples p]
            (reduce (partial collect-o s p)
                    triples
                    (g s p)))
          (collect-s-p-o [triples s]
            (reduce (partial collect-p-o s)
                    triples
                    (keys (g s))))

          ]
    (with-meta
      (reduce collect-s-p-o
              []
              (keys (g)))
      {:triples-format :vector-of-vectors})))


(defmethod add-to-graph [Graph :normal-form] [g to-add]
  (letfn [(collect-key [m acc k]
            (assoc acc k
                   (if (contains? acc k)
                     (merge-tree(acc k)
                                (m k))
                     (m k))))
          (integrate [acc m]
            (reduce (partial collect-key m) acc (keys m)))
          (merge-tree [m1 m2]
            (if (set? m1)
              (set/union m1 m2)
              (-> {}
                  (integrate m1)
                  (integrate m2))))
        ]
  (make-graph
   :schema (.schema g)
   :contents (merge-tree (g) to-add))))

(defmethod add-to-graph [Graph :vector-of-vectors] [g triples]
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

(defmethod add-to-graph [Graph :vector] [g triple]
  (assert (= (count triple) 3))
  (add-to-graph g [triple]))

(defmethod add-to-graph [Graph clojure.lang.LazySeq] [g the-seq]
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


(defn- shared-keys [m1 m2]
  "Returns {<shared key>...} for <m1> and <m2>
Where
<shared key> is a key in both maps <m1> and <m2>
"
  (set/intersection (set (keys m1))
                    (set (keys m2))))

(defmethod remove-from-graph [Graph :normal-form] [g to-remove]
  (letfn [(dissoc-in [shared-path acc value]
            (let [shared-path (conj shared-path value)
                  ]
              (-dissoc-in acc shared-path)))
          
          (dissoc-shared-keys [shared-path acc next-key]
            (let [shared-path (conj shared-path next-key)
                  v1 (get-in (g) shared-path)
                  v2 (get-in to-remove shared-path)
                  ]
              (if (set? v1)
                (reduce (partial dissoc-in shared-path)
                        acc
                        (set/intersection v1 v2))
                (reduce (partial dissoc-shared-keys shared-path)
                        acc
                        (shared-keys v1 v2)))))
          ]
    (make-graph
     :schema (.schema g)
     :contents (reduce (partial dissoc-shared-keys [])
                       (g)
                       (shared-keys (g) to-remove)))))

(defmethod remove-from-graph [Graph :vector-of-vectors] [g triples]
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

(defmethod remove-from-graph [Graph :vector] [g to-remove]
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



(defn get-intersection
  "Returns a new graph whose triples are shared between `g1` and `g2`
  Where
  <g1> and <g2> both implement IGraph.
  "
  [g1 g2]
  (let [collect-p
        (fn [s acc p]
          (assoc-in acc
                    [s p]
                    (set/intersection
                     (set (get-in (g1) [s p]))
                     (set (get-in (g2) [s p])))))
        collect-s
        (fn [acc s]
          (reduce (partial collect-p s)
                  acc
                  (shared-keys (g1 s) (g2 s))))
        ]
    (make-graph
     :schema (.schema g1)
     :contents (reduce collect-s {} (shared-keys (g1) (g2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for simple queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 
(defn query-var?
  "Returns true iff <spec> is a query variable (symbol whose name
  starts with ?)
  "
  [spec]
  (and (keyword? spec)
       (not (nil? (re-matches #"^\?.*" (name spec))))))

(defn- -matches-spec?
  "
  Returns true if <target> matches <spec> in <query context>
  Where
  <target> is an element in some Graph being match-tested in some query.
  <spec> := is an element in some query clause
  <query-context> := (keys %) -> #{maybe :var-tests ...} , typically
    the 'context' argument in an -X-matches function.
  <var-tests> := {<var> (fn[target]...) -> true if <target> matches <var>, ...}
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
  "Returns <match'> modified per <spec> and <value> only if (query-var? <spec>)
  Where
  <match> := {:matched? <boolean>, <var> <value> ...}, typically acquired
    matching a graph pattern
  <spec> is an element of a graph pattern.
  <var> is a variable <spec> which matched <value>
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
  Returns <match> to <next-o> in <context> querying <g>
  Where
  <match> := {:matched? <var>, <var> <o>, ... }
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
  <match-results> := [<match> ...]
  <match> := {:matched? <matched?>, <var> <value>, ...}
  <matched?> is true iff <p> matched <next-p> and <o> matched downstream
  <g> is a Graph
  <context> :=  {:s <s> :spec <spec>}
  <s> is a subject element of <g> and the <s> argument in some graph pattern
    clause
  <spec> := [<p> <o>] derived from some graph pattern clause.
  "
  [^Graph g
   ^clojure.lang.PersistentArrayMap context
   ^clojure.lang.PersistentVector next-p]
  (let [[p-spec o-spec] (:spec context)
        ]
    (if (not (-matches-spec? context p-spec next-p))
      {:matched? false}
      ;; else it matches <p>...
      (map (partial -annotate-match p-spec next-p)
           ;; try to match on <o> ...
           (filter :matched?
                   (map (partial -o-match
                                 g
                                 (assoc context
                                        :p next-p ;; p-spec
                                        :spec [o-spec]))
                        (g (:s context) next-p)))))))

(defn- -s-p-o-matches
  "Returns <match-results> for <g> <context> and <next-s>
  Where
  <match-results> := [<match> ...]
  <match> := #{:matched? <matched?>, <var> <value>,...}
  <matched?> is true iff <s> matched <next-s> and <p> and <o> matched downstream
  <g> is a Graph
  <context> := {:spec <spec> :var-tests <var-tests>}
  <spec> := [<s-spec> <p-spec> <o-spec>], a query triple.
  <s-spec> specifies a match to subjects in the graph pattern clause
  <var-tests> := {<var> <test> ...}
  <test> := (fn [next-s]) -> true if next-s cannot be excluded from matching
    in the current context.
  "
  [^Graph g
   ^clojure.lang.PersistentArrayMap context
   next-s]
  (let [[s-spec p-spec o-spec] (:spec context)
        ]
    (if (not (-matches-spec? context s-spec next-s))
      {:matched? false}
      (map (partial -annotate-match s-spec next-s)
           (filter :matched?
                   (if (fn? p-spec) ;; traversal-fn
                     (map (partial -o-match
                                   g
                                   (assoc context
                                          :s next-s
                                          :spec [o-spec]))
                          (traverse g p-spec #{} [next-s]))
                     ;; else it's not a traversal function...
                     (mapcat (partial -p-o-matches
                                      g
                                      (assoc context
                                             :s next-s
                                             :spec [p-spec o-spec]))
                             (keys (g next-s)))))))))


(defn- -query-clause-matches
  "Returns <matches> for <clause> posed against <g> using <var-tests>
  Where
  <matches> := [<match> ...]
  <clause> :=[<s-spec> <p-spec> <o-spec>], a line from a simple query
  <g> is an instance of Graph.
  <var-tests> := {<var> (fn [value] ....) -> true if <value> matches <var>, ...}
  <match> := {:matched? <matched?>, <var> <value>, ...}
  <s-spec> and <o-spec> are either variables or graph elements to match in
    the graph pattern
  <p-spec> is either a variable, a graph element to match in the graph, or a
    traversal function := (fn [g acc queue]...) -> [acc' queue'] s.t.
    (traverse g p #{} s) -> #{<o> ....}
  <matched?> is true iff <clause> matches the contents of the graph per
    <var-tests>
  <var> names the subset of <s> <p> <o> for which (query-var? %) is true
  <value> is an element in <g> s.t. ((<var> <var-tests>) <value>)
  "
  ([^Graph g
    ^clojure.lang.PersistentVector clause
    ^clojure.lang.PersistentArrayMap var-tests]
   (let [[s-spec p-spec o-spec] clause
         ]
     (map #(dissoc % :matched?)
          (filter :matched?
                  (mapcat  (partial -s-p-o-matches g {:var-tests var-tests
                                                      :spec clause})
                           (if (query-var? s-spec)
                             (keys (.contents g))
                             [s-spec]))))))
  ;; no var-tests specified
  ([g clause]
   (-query-clause-matches g clause {})))


(defn- -collect-clause-match
  "Returns <clause-state> modified for <match> to <g> in <query-state>
  Where
  <clause-state> := {:bindings <bindings> :shared-bound <shared-bound>}
    s.t. <bindings> membership is appropriately modified per <match>
  <match> := {:matched? <match-matched?>, <var> <value>, ...}  
  <query-state> := {:bindings <bindings>
                    :specified <specified>
                    :matched? <state-matched?>},
    modified s.t. each <match> found for <clause> is joined with compatible
    existing matches.
  <bindings> := #{<binding>...} typically to some query clause
  <binding> is a valid <match> integrating compatible matches from all previous
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

       
