

(ns ^{:author "Eric D. Scott",
      :doc "Implementation of a simple graph type implementing IGraph.
On typically adds to it with [[`s` `p` `o`]...].
One queries it with a simple graph pattern of the form [[`s` `p` `o`]...]
With variables of the form :?x.

The core type declaration:

```
(deftype Graph [contents]
  
  IGraph
  (normal-form [g] (.contents g))
  (subjects [g] (lazy-seq (keys (.contents g))))
  (get-p-o [g s] (get (.contents g) s))
  (get-o [g s p] (get-in (.contents g) [s p]))
  (ask [g s p o] (get-in (.contents g) [s p o]))
  (query [g q] (query-graph g q))
  (add [g to-add] (add-to-graph g to-add))
  (subtract [g to-subtract] (remove-from-graph g to-subtract))
  
  clojure.lang.IFn
  (invoke [g] (normal-form g))
  (invoke [g s] (get-p-o g s))
  (invoke [g s p] (match-or-traverse g s p))
  (invoke [g s p o] (match-or-traverse g s p o))

  IGraphSet
  (union [g1 g2] (add-to-graph g1 (g2)))
  (intersection [g1 g2] (get-intersection g1 g2))
  (difference [g1 g2] (remove-from-graph g1 (g2)))
  )
```
"}
    ont-app.igraph.graph
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as spec]
            [ont-app.igraph.core
             :as igraph
             :refer
             [
              add
              add-to-graph
              get-o
              get-p-o
              match-or-traverse
              normal-form
              reduce-spo
              remove-from-graph
              subjects
              traverse
              triples-format
              unique
              ]
             ]
            ))

(declare query-graph) ;; defined below
(declare get-intersection)
(declare get-contents)

(deftype Graph [contents]

  igraph/IGraph
  (normal-form [g] (get-contents g)) 
  (subjects [g] (lazy-seq (keys (get-contents g))))
  (get-p-o [g s] (get (get-contents g) s))
  (get-o [g s p] (get-in (get-contents g) [s p]))
  (ask [g s p o] (get-in (get-contents g) [s p o]))
  (query [g q] (query-graph g q))
  (mutability [_g_] ::igraph/immutable)
  
  #?(:clj clojure.lang.IFn
     :cljs cljs.core/IFn)
  (#?(:clj invoke :cljs -invoke) [g] (normal-form g))
  (#?(:clj invoke :cljs -invoke) [g s] (get-p-o g s))
  (#?(:clj invoke :cljs -invoke) [g s p] (match-or-traverse g s p))
  (#?(:clj invoke :cljs -invoke) [g s p o] (match-or-traverse g s p o))

  igraph/IGraphImmutable
  (add [g to-add] (add-to-graph g to-add))
  (subtract [g to-subtract] (remove-from-graph g to-subtract))
  
  igraph/IGraphSet
  (union [g1 g2] (add-to-graph g1 (g2)))
  (intersection [g1 g2] (get-intersection g1 g2))
  (difference [g1 g2] (remove-from-graph g1 (g2)))
  )

(defn get-contents 
  "Returns (.contents g) or (.-contents g) appropriate to clj/cljs"
  [^Graph g]
  #?(:clj
     (.contents g)
     :cljs
     ^PersistentArrayMap (.-contents g)))

(def cljc-lazy-seq
  "Platform-agnostic LazySeq."
  #?(:clj clojure.lang.LazySeq
     :cljs cljs.core/LazySeq
     ))

;; NO READER MACROS BELOW THIS POINT
  
  
(defn make-graph
  "Returns `graph`, intialized per optional `contents`
  Where
  -   `graph` is an instance of the `Graph` type, which implments `IGraph`, `Ifn` and `ISet`
  -   `contents` is a normal-form representation of initial contents.
  SEE ALSO: igraph/normal-form.
  "
  ([&{:keys [contents]
      :or {contents {}}}]
   {:pre [(= (triples-format contents) :normal-form)]
    }
   (Graph. (with-meta contents {::igraph/triples-format :normal-form}))
   ))


(defn vector-of-triples 
  "Returns (g) as [[`s` `p` `o`]...]"
  [g]
  (with-meta
    (reduce-spo
     (fn [v s p o] (conj v [s p o]))
     []
     g)
    {::igraph/triples-format :vector-of-vectors}))


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
     :contents (merge-tree (g) to-add))))


(defmethod add-to-graph [Graph :vector-of-vectors]
  [g triples]
  ;; Where `triples` := [`v` ....]
  ;; `v` := [`s` `p1` `o1` `p2` `o2` ...`pn` `on`] 
  (let [collect-triple (fn [s acc [p o]]
                         (update-in acc
                                    [s p]
                                    #(conj (set %) o)))                       
        collect-vector (fn [acc v]
                         (reduce (partial collect-triple (first v))
                                 acc
                                 (partition 2 (rest v))))
        ]
    (make-graph
     :contents (reduce collect-vector (get-contents g) triples))))

(defmethod add-to-graph [Graph :vector] [g triple-spec]
  (if (empty? triple-spec)
    g
    (add-to-graph g
                  ^{::igraph/triples-format :vector-of-vectors}
                  [triple-spec])))

(defmethod add-to-graph [Graph cljc-lazy-seq]
  [g the-seq]
  (if (empty? the-seq)
    g
    (add-to-graph g
                  ^{::igraph/triples-format :vector}
                  (vec the-seq))))


(defn- -dissoc-in 
  "removes the last key in `path` from its parent in `map-or-set`, removing
    any empty containers along the way.
Where 
  - `map-or-set` is typically a sub-tree of graph contents
  - `path` := [`key` ...]
Note: typically used to inform removal of nodes in a graph, where `key` is 
  a subject, predicate or object
"
  [map-or-set path]
  (let [key (first path)
        ]
    (assert (seq path))
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


(defn- shared-keys 
  "Returns {`shared key`...} for `m1` and `m2`
Where
  - `shared key` is a key in both maps `m1` and `m2`
"
  [m1 m2]
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
    (if (empty? to-remove)
      g
      (make-graph
       :contents (reduce (partial dissoc-shared-keys [])
                         (g)
                         (shared-keys (g) to-remove))))))

(defmethod remove-from-graph [Graph :vector-of-vectors]
  [g triples]
  ;; Where `triples` := [`v` ....]
  ;; `v` := [`s` `p1` `o1` `p2` `o2` ...`pn` `on`] , or [`s`] or [`s` `p`]
  ;; `s` is a subject in `g`
  ;; `p` is a predicate for `s` in `g`
  ;; `o` is an object for `s` and `p` in `g`
  ;; Note: 
  ;; `v` = [`s`] signals that all {`p` `o`} s.t. (`g` `s`) should be removed.
  ;; `v` = [`s` `p`] signals that all `o` s.t. (`g` `s` `p`) should be removed
  (let [remove-triple (fn [s acc [p o]]
                        (-dissoc-in acc [s p o]))
        collect-vector (fn [acc v]
                         (if (< (count v) 3) ;; specifies s or s-p
                           (-dissoc-in acc v)
                           ;; else this specifies one or more triples...
                           (reduce (partial remove-triple (first v))
                                   acc
                                   (partition 2 (rest v)))))
        ]
    (if (empty? triples)
      g
      ;; else
      (make-graph
       :contents (reduce collect-vector (get-contents g) triples)))))

#_(defmethod old_remove-from-graph [Graph :vector]
  [g to-remove]
  ;; Where
  ;; `to-remove` may be [s] [s p] [s p o]
  (if (empty? to-remove)
    g
    (make-graph
     :contents (-dissoc-in (get-contents g)
                           to-remove))))

(defmethod remove-from-graph [Graph :vector]
  [g to-remove]
  ;; Where
  ;; `to-remove` may be [s] [s p] [s p o], or [s p1 o1, p2 o2, ...]
  (if (empty? to-remove)
    g
    (make-graph
     :contents (if (<= (count to-remove) 3)
                 (-dissoc-in (get-contents g)
                             to-remove)
                 ;; else this is a long vector
                 (let [diminish-contents (fn [s acc [p o]]
                                           (-dissoc-in acc [s p o]))
                       ]
                   (reduce (partial diminish-contents (first to-remove))
                           (get-contents g)
                           (partition 2 (rest to-remove))))))))


(defmethod remove-from-graph [Graph :underspecified-triple]
  [g to-remove]
  ;; Underspecified-vector is a distinction without a difference at this point
  (let [f (get-method remove-from-graph [Graph :vector])]
    (f g to-remove)))


(defn- get-intersection
  "Returns a new graph whose triples are shared between `g1` and `g2`
  Where
  -   `g1` and `g2` both implement IGraph.
  "
  [g1 g2]
  (let [collect-p
        (fn [s acc p]
          (let [_intersection
                (set/intersection
                 (set (get-in (g1) [s p]))
                 (set (get-in (g2) [s p])))
                ]
            (if (empty? _intersection)
              acc
              (assoc-in acc
                        [s p]
                        _intersection))))
        collect-s
        (fn [acc s]
          (reduce (partial collect-p s)
                  acc
                  (shared-keys (g1 s) (g2 s))))
        ]
    (make-graph
     :contents (reduce collect-s {} (shared-keys (g1) (g2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for simple queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 
(defn- kw-starts-with-?
  "Returns true iff `spec` is a symbol whose name
  starts with ?. Default for query-var? parameter
  "
  [spec]
  (and (keyword? spec)
       (not (nil? (re-matches #"^\?.*" (name spec))))))

;; ^reduce-fn
(defn- -collect-o-match 
  "
  Adds new `match` to `matches` for `next-o` in `context`
  Where
  -   `match` := {`var` `o`, ... }, as set of variable bindings
  -   `matches` := [`match`, ...]
  -   `next-o` is a graph element, the final element of a matching process for some
      query clause
  - `context` is a graph representing the match context for some query clause
      := {:s `fixed-desc` or `elt-desc`
          :p `fixed-desc` or `elt-desc`
          :o `open-desc`  or `elt-desc` 
  - `fixed-desc` := {:bound-to #{`var`}
                   :value #{`value`}
                  }
     ... assigns one of the matching values encountered upstream
  
  -   `elt-desc` := {:value #{`value`}
                   :candidate #{`value`}
                  }
      ... already matched to a fixed value specified in the clause itself

  -   `open-desc` := {:bound-to `var`
                    :candidate #{`value`, ...}
       ... waiting to be matched to values downstream
  -   `var` is a variable
  -   `value` is a graph element, a singleton subset of #{`candidate`, ...}
      if candidates are specified.
  -   `candidate` may be specified at the start of the clause matching based
      on whether `var` was matched upstream in the query context
  "
  [context matches next-o]
  (let [context (cond-> context ;; if o is a var, set next-o as value for var
                  (context :o :bound-to)
                  (add [:o :value next-o]))
        collect-bindings (fn [b spo]
                           ;; add value of s|p|o if it's a var
                           ;; spo is one of #{:s :p :o}
                           (if-let [var (unique (context spo :bound-to))]
                             (assoc b var (unique (context spo :value)))
                             b))
        
        ]
    (conj matches (reduce collect-bindings {} [:s :p :o]))))


;; ^reduce-fn
(defn- -collect-p-o-matches
  "Returns `matches`' for `g` in `context` given `next-p`
  Where
  -   `matches` := #{`match` ...}
  -   `match` := {`var` `value`, ...}
  -   `var` is a variable bound to either :s :p  or :o in `context`
  -   `value` is a value associated with `var` in `g`
  -   `g` is a Graph
  -   `next-p` is either a graph element or a traversal function
  -   `context` is a graph representing the match context for some query clause
      := {:s `fixed-desc` or `elt-desc`
          :p `open-desc` or `elt-desc`
          :o `open-desc` or `elt-desc`
         }
  -   `fixed-desc` := {:bound-to #{`var`}
                     :value #{`value`}
                    }
       ... assigns one of the matching values encountered in the course
       of matching.
  -  `elt-desc` := {:value #{`value`}
                   :candidate #{`value`}
                  }
      ... already matched to a fixed value specified in the clause itself
  -   `open-desc` := {:bound-to `var`
                      :candidate #{`value`, ...}
       ... waiting to be matched to values downstream
  -   `var` is a variable
  -   `value` is a graph element, a singleton subset of #{`candidate`, ...}
      if candidates are specified.
  - `candidate` may be specified at the start of the clause matching based
      on the upstream query context.
  "
  [^Graph g ^Graph context matches next-p]
  {:pre [(set? matches)
         ]
   }
  (let [o-candidates (context :o :candidate)
        ;; ... bound to the <o` in previous clauses, or nil
        qualify-os (fn [os]
                     ;; returns #{<o`} s.t. <o` is consistent with o's
                     ;; acquired in clauses upstream
                     (if o-candidates
                       (set/intersection o-candidates os)
                       os))
        ]

    (if (fn? next-p)
      ;;traversal fn, match against the o's it acquires, modulo var bindings
      ;; from upstream
      (reduce (partial -collect-o-match context)
              matches
              (qualify-os (traverse
                           g
                           next-p
                           #{}
                           [(unique (context :s :value))])))

      ;; else p is a graph element, match against the o's that next-p is
      ;; associated with, modulo var bindings from upstream
      (reduce (partial -collect-o-match
                       ;; update p-specifications of context
                       (if (context :p :bound-to)
                         ;; <p` is a variable, specify this graph element
                         (do
                           (assert (not (context :p :value)))
                           (add context 
                                [[:p :value next-p]]))
                         (do
                           ;; <p` is  a graph element
                           (assert (context :p :value))
                           context)))
              matches
              ;; match the objects for <s` and <next-p`
              (qualify-os (g (unique (context :s :value)) next-p))))))
                

(defn- -collect-s-p-o-matches
  "Returns `matches` for `next-s` in `g`, given `context`
  Where
  -   `matches` := [`match` ...]
  -   `next-s` is a subject matching the current clause in some query.
  -   `match` := #{:`var` `value`,...}
  -   `g` is a Graph
  -     `context` is a graph representing the match context for some query clause
      := {:s `open-desc` or `elt-desc`
          :p `open-desc` or `elt-desc`
          :o `open-desc` or `elt-desc`
         }
  -   `open-desc` := {:bound-to #{`var`} 
                      :candidate #{`candidate`, ...} or nil if anything matches
      ... waiting to be matched to values downstream
  -   `elt-desc` := {:value #{`value`}
                     :candidate #{`value`}
                  }
      ... already matched to a fixed value specified in the clause itself
  -   `var` is a :?variable
  -   `candidate` may be specified at the start of the clause matching based
        on whether `var` was bound upstream in the query context
  "
  [^Graph g ^Graph context matches next-s]  
  {:pre [(set? matches)]
   }
  (letfn [(qualify-ps
            [ps]
            ;; limits ps to specified candidates for p, if they exist
            (if-let [candidates (context :p :candidate)]
              (set/intersection candidates ps)
              ps))]

    (reduce (partial -collect-p-o-matches
                     g
                     (if (context :s :bound-to)
                       (do
                         (assert (not (context :s :value)))
                         (add context
                              [[:s :value next-s]]))
                       ;; else s is not bound to var
                       (do
                         (assert (context :s :value))
                         context)))
            matches
            (if (fn? (unique (context :p :value)))
              (context :p :value)
              ;; else p is a graph element
              (qualify-ps (set (keys (g next-s))))))))


(defn- -query-clause-matches
  "Returns `matches` for `clause` posed against `g` in `query-context`
  Where
  -   `matches` := [`match` ...]
  -   `clause` :=[`s-spec` `p-spec` `o-spec`], a line from a simple query
  -   `g` is an instance of Graph.
  -   `query-context` := {:query-var? ..., ...}
  -   `match` := { `var` `value`, ...}
  -   `s-spec` and `o-spec` are either variables or graph elements to match in
      the graph pattern
  -   `p-spec` is either a variable, a graph element to match in the graph, or a
      traversal function := (fn [g acc queue]...) -> [acc' queue'] s.t.
      (traverse g p #{} s) -> #{`o` ....}
  -   `var` names the subset of `s` `p` `o` for which (query-var? %) is true
  -   `value` is an element in `g` which matches some `var` in `clause`
  -   `query-var?` := (fn [var]...)  -> true iff `var` is a variable.
  "
  [^Graph g ^Graph query-context clause]
  {:pre [(map? query-context)
         (= (count clause) 3)]
   }
  (let [
        [s-spec p-spec o-spec] clause
        query-var? (:query-var? query-context)
        candidates-for (fn [q-var]
                         ;; returns the set of values already bound
                         ;; to any var upstream. the current clause
                         ;; must bind to a subset of these
                         (if-let [cs (-> query-context
                                         :specified
                                         (q-var)
                                         (keys))]
                           (set cs)))

        
        add-candidates (fn [c spo q-var]
                         ;; Updates context <c> s.t. spo *may* have a set of
                         ;; candidate bindings.
                         (let [candidates
                               (candidates-for q-var)]
                           (if (and candidates (seq candidates))
                             (add c {spo {:candidate candidates}})
                             c)))

        add-var-context (fn [c spo q-var]
                          ;; updates context <c> s.t. variable bindings
                          ;; for s p or o are accounted for
                          (-> c
                              (add [spo :bound-to q-var])
                              (add-candidates spo q-var)))
        
        add-graph-element-context (fn [c spo element]
                                    ;; updates the context for the case
                                    ;; where s p or o is a graph element
                                    ;; -- or maybe a traversal fn in the case
                                    ;; of p
                                    (-> c
                                        (add [[spo :value element]
                                              [spo :candidate element]
                                              ]
                                             )))
        update-clause-context (fn [c spo spec]
                                ;; updates the context appropriately
                                ;; for either a variable or a graph element
                                (cond->
                                    c
                                  (query-var? spec)
                                  (add-var-context spo spec),
                                  
                                  (not (query-var? spec))
                                  (add-graph-element-context spo spec)))

        clause-context (-> (make-graph)
                           ;; Updates the context for s p and o
                           (update-clause-context :s s-spec)
                           (update-clause-context :p p-spec)
                           (update-clause-context :o o-spec))
        
        ]
    (reduce (partial -collect-s-p-o-matches
                     g
                     clause-context)
            #{}
            ;; reducing over the set of subjects associable
            ;; with <s>.
            (if (query-var? s-spec)
              (or (candidates-for s-spec)
                  ;; Expensive for large graphs ...
                  (let [ss (set (subjects g))]
                    (if (fn? p-spec) ;; may be a closure with path of 0 len
                      (if (var? o-spec)
                        (set/union ss (candidates-for o-spec))
                        ;; else o-spec is a graph element
                        (set/union ss #{o-spec}))
                      ;; else not a fn, path is of length 1
                      ss)))
              ;; TODO: consider introducing an indexing facility
              ;; else s-spec is a graph element
              [s-spec]))))

(defn- -collect-clause-match
  "Returns [`match`...] for `context` and `match`
  Where
  -   `clause-state` := {:bindings `bindings` :shared-bound `shared-bound`}
        s.t. `bindings` membership is appropriately modified per `match`
        meaning that that members of `bindings` inconsistent with `match`
        have been removed, along with their corresponding entries in
        `specified`
  -   `match` := {`var` `value`, ...}  
  -   `query-state` := {:bindings `bindings`
                        :specified `specified`
                        ...
                        },
    modified s.t. each `match` found for `clause` is joined with compatible
    existing matches.
  -   `bindings` := #{`binding`...} typically to some query clause
  -   `binding` is a valid `match` integrating compatible matches from all previous
        clauses
  -   `shared-bound` := #{`var` ...}, s.t. `var` is bound in `specified`, and
        also present in the current graph pattern clause. This means new bindings
        must be unified with matches already specified upstream.
  -   `specified` := {`var` {`value` `specified bindings` ...}...}, a Graph
  -   `specified bindings` := #{`specified match`...} a subset of `matches` for which
  -   `var` was bound to `value` in previous clauses.
  "
  [query-state clause-state match]
  {:pre [(map? query-state)
         (map? clause-state)
         (map? match)
         ]
   }
  (assoc clause-state
         :bindings
         (set/union
          (or (:bindings clause-state) #{})
          (if (seq (:shared-bound clause-state))
            (set (map (partial merge match)
                      (reduce set/intersection
                              (map (fn [qvar]
                                     (-> query-state
                                         :specified 
                                         (get-o qvar (qvar match))))
                                   (:shared-bound clause-state)))))
                                              
            ;;else the bindings in this clause have no precedent
            (if (:bindings query-state)
              (set (map (partial merge match)
                        (:bindings query-state)))
              ;;else there are no bindings in the query state
              #{match})))))


(defn- -triplify-binding 
  "Returns [[`var` `value` `binding`]...] for `binding`, given `query-var?`
Where
  -   `binding` := {`var` `value` ...}
  -   `query-var?` := (fn [var] ...) -> true iff `var` is a variable.
  -   `var` is a query-var
  -   `value` is typically a value bound to `var` in some query match
  NOTE: this is typically used to populate the 'specified' graph in 
  a query-state, which informs the matching process downstream.
  "
  [query-var? binding]
  {:pre [(map? binding)]
   }
  (let [triplify-var (fn [binding qvar]
                       [qvar (qvar binding) binding])
        ]
    (vec (map (partial triplify-var binding)
              (filter query-var? (keys binding))))))

;; ^reduce-fn
(defn- -collect-clause-matches
  "Returns `query-state` modified for matches to `clause` in `g`
  Where
  -   `query-state` := {:viable? ... :matches ... :specified ...}
         modified s.t. each match found for clause is joined with compatible
         existing matches.
  -   `clause` is a triple of query specs, typically part of some query
  -   `g` is a Graph
  -   `viable?` is true if no upstream clauses have failed. Matching ceases
         in the event that the query is no longer viable.
  -   `matches` := #{`match`...} which match this and upstream `clause` in `g`
  -   `match` a valid match integrating compatible matches from this and all previous
        clauses
  -   `specified` := {`var` {`value` `specified matches` ...}...}, a Graph
        `specified matches` := #{`specified match`...} a subset of `matches` containing
        a binding for `var` and `value`
  "
  [^Graph g query-state next-clause]
  {:pre [(map? query-state)
         (vector? next-clause)
         (= (count next-clause) 3)]
   }

  (if-not (:viable? query-state)
    query-state
    ;; else the query is still viable
    (let [query-var? (:query-var? query-state)
          initial-clause-state
          {
           ;; Collect all the variables in this clause which are shared
           ;; with matches in previous clauses, so we can by unified
           ;; with consistent new matches in the next clause
           :shared-bound (set/intersection
                          (or (and (:specified query-state)
                                   (set (-> query-state
                                            :specified
                                            normal-form
                                            keys))) 
                              #{})
                          (set (filter query-var? next-clause)))
           }
          clause-state 
          (reduce (partial -collect-clause-match query-state)
                  initial-clause-state
                  (-query-clause-matches g query-state next-clause))
          ]
      (if-let [bindings (:bindings clause-state)]
        (assoc query-state
               :bindings bindings
               :specified (add (make-graph)
                               (into []
                                     (mapcat (partial -triplify-binding
                                                      query-var?)
                                             (:bindings clause-state)))))
        (assoc query-state
               :viable? false
               :bindings nil
               )))))

(defn query-graph
  "Returns #{`binding`...} for `graph-pattern` applied to `g`
  Where
  -   `g` is a Graph
  -   `graph-pattern` := [[`var-or-value` `var-or-value` `var-or-value`]...]
  -   `var-or-value` is in #{`var` `value`}
  -   `var` is a keyword whose name begins with '?'
  -   `value` is a value which must match an element of `g` exactly.
  -   `binding` := {`var` `matching-value`, ...}
  -   `matching-value` matches `var` within `graph-pattern` applied to `g`
  "
  ([^Graph g graph-pattern query-var?]
  {:pre [(vector? graph-pattern)
         (vector? (graph-pattern 0))]
   }
  (or (:bindings
       (reduce (partial -collect-clause-matches g)
               {:viable? true
                :query-var? query-var?
                }
               graph-pattern))
      #{}))
  ([^Graph g graph-pattern]
   (query-graph g graph-pattern kw-starts-with-?)))

       
(comment
  )
