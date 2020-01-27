(ns ^{:author "Eric D. Scott",
      :doc "Abstractions over a graph object, intended to sit alongside the 
other basic clojure data structures such as maps, vectors and sets.
"}
    ont-app.igraph.core
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.spec.alpha :as spec]
            [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            ))

;; FUN WITH READER MACROS

#?(:cljs
   (enable-console-print!)
   )

#?(:cljs
   (defn on-js-reload [] )
   )

(declare normal-form)
#?(:clj
   (defn write-to-file [path g]
     "Side-effect: writes normal form of `g` to <path> as edn.
Returns: <path>
Where
<path> is the output of <path-fn>
<g> implements IGraph
<path-fn> a function [g] -> <path>.
NOTE: Anything that would choke the reader on slurp should be removed 
  from <g> before saving.
"
     (let [output-path (str/replace path #"^file://" "")
           ]
       (io/make-parents output-path)
       (spit output-path
             (with-out-str
               (pp/pprint
                (normal-form g))))
       output-path
       )))
(declare add)
#?(:clj
   (defn read-from-file [g path]
     "returns `g` with the contents of `path` added
Where
<g> implements IGraph
<path> is an edn file containing a normal-form representation of some graph,
typically the output of save-to-file."
     (add g (read-string (slurp (io/as-file path))))
     ))

;; No reader macros below this point

(defprotocol IGraph
  "An abstraction for S-P-O graphs"

  ;;;;;;;;;;;;;;;;;;;;
  ;; ACCESS FUNCTIONS
  ;;;;;;;;;;;;;;;;;;;;
 
  (normal-form [g] "Returns {<s> {<p> #{<o>...}...}...}
Where 
<s> is the subject of a triple := [<s> <p> <o>] in <g>
<p> is predicate of same
<o> is the object of same
")
  (subjects [g]
    "Returns (<s>...) for <g>
Where 
<s> is a subject in one or more triples in <g>
<g> is a graph.
"
    )
  (get-p-o [g s]
    "Returns {<p> #{<o> ...}} associated with <s> in <g>, or nil.
Where
<g> is a graph
<s> is subject 
<p> and <o> are in triples := [<s> <p> <o>] in <g>
"
    )
  (get-o [g s p]
    "Returns {<o> ...} for <s> and <p> in <g>, or nil.
Where
<g> is a graph
<s> is subject of some triples in <g>
<p> is predicate of some triples in <g>
<o> appears in triple [<s> <p> <o>] in <g>
"
    )
  (ask [g s p o]
    "Returns truthy value iff [<s> <p> <o>] appears in <g>
Where
<g> is a graph
<s> is subject of some triples in <g>
<p> is predicate of some triples in <g>
<o> appears in triple [<s> <p> <o>] in <g>
"
    )
  (query [g q]
    "Returns #{<binding> ...} for query spec <q> applied to <g>
Where
<binding> := {<var> <value>, ...}
<q> is a query specification suitable for the native format of <g>
<g> is a graph
<args> := [<arg>....]
<var> is a variable specified in <q>
<value> is a value found in <g> bounded to <var> per <q>
<arg> is any optional value that informs native execution of the query.
  for example if the native platform supports a templating scheme as in
  datalog
"
    )
  ;; for IFn
  (invoke [g] [g s] [g s p] [g s p o]
    "Applies <g> as a function to the rest of its arguments, representing 
   triples [<s> <p> <o>] in <g> respectively. <p> may optionally be 
   a traversal function (See `traverse` docs)
(g) -> {<s> {<p> #{<o>...}...}...} ;; = (normal-form <g>)
(g s) -> {<p> #{<o>...}, ...} ;; = (get-p-o <g>)
(g s p) -> #{<o> ...} ;; = (match-or-traverse g s p)
(g s p o) -> <o> iff [<s> <p> <o>] is in <g> ;; = (match-or-traverse g s p o)
")
  ;; mutability
  (mutability [g]
    "Returns one of ::read-only ::immutable ::mutable ::accumulate-only"
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CONTENT MANIPULATION
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprotocol IGraphImmutable
    (add [g to-add]
    "Returns <g>', with <to-add> added to its contents.
Throws a ::ReadOnly exception if (read-only? <g>)
Where
<g> is a graph
<to-add> is in triples-format
"
    )
  (subtract [g to-subtract]
    "Returns <g>' with <to-subtract> removed from its contents.
Throws an exception if (mutability g) != ::immutable
Where
<g> is an immutablegraph
<to-subtract> is in triples-removal-format
"
    ))

(defprotocol IGraphMutable
    (add! [g to-add]
    "Returns <g>, with <to-add> added to its contents.
Throws an exception if (mutability g) != ::mutable
Where
<g> is a mutable graph
<to-add> is in triples-format
"
    )
  (subtract! [g to-subtract]
    "Returns <g> with <to-subtract> removed from its contents.
Throws a ::ReadOnly exception if (read-only? <g>)
Where
<g> is a graph
<to-subtract> is in triples-removal-format
"
    ))

(defprotocol IGraphAccumulateOnly
    (claim [g to-add]
    "Returns <g>, with <to-add> added to <g>'s associated transactor.
Throws an exception if (mutability g) != ::accumulate-only
Where
<g> is a mutable graph
<to-add> is in triples-format
NOTE: see Datomic documentation for the 'add' operation for details
"
    )
  (retract [g to-retract]
    "Returns <g> with <comm> reset to head
Side-effect:  <to-retract> retracted from <comm>
Throws an exception if (mutability g) != ::accumulate-only.
Where
<g> is a graph
<comm> is a datomic-style transactor
<to-retract> is in triples-removal-format
NOTE: see Datomic documentation for details
"
    ))


(defprotocol IGraphSet
  "Basic set operations between graphs."
  (union [g1 g2]
    "Returns an IGraph whose normal form contains all triples from g1 and g2"
    )
  (intersection [g1 g2]
    "Returns an IGraph whose normal form contains all and only statements shared by both g1 and g2"
    )
  (difference [g1 g2]
    "Returns an IGraph whose normal form contains all statements in g1 not present in g2."
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MULTI-METHODS FOR ALTERING GRAPHS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn normal-form? 
  "Returns true iff <m> is in normal form for IGraph."
  ;; TODO: port to clojure.spec
  [m]
  (and (map? m) 
       (or (empty? m)
           (let [p (m (first (keys m)))
                 o (p (first (keys p)))
                 ]
             (set? o)))))

(spec/def ::normal-form #(normal-form? %))
(spec/def ::vector (spec/and vector? #(> (count %) 1) #(odd? (count %))))
(spec/def ::vector-of-vectors (spec/and vector? (spec/every ::vector)))
(spec/def ::triples-format (spec/or :vector-of-vectors ::vector-of-vectors
                                    :vector ::vector
                                    :normal-form ::normal-form))
        
(defn triples-format 
  "Returns the value of (:triples-format (meta <triples-spec>)) or one of #{:vector :vector-of-vectors :normal-form <type>} inferred from the shape of <triples-spec>
  Where
  <args> := [<g> <triples-spec>],  arguments to a method add or remove from graph
  <g> is a graph
  <triples-spec> is a specification of triples typically to add to or remove
    from  <g>
  :normal-form indicates (normal-form? <triples-spec>) = true
  :triple indicates <triples-spec> := [<s> <p> <o>]
  :vector-of-vectors indicates <triples-spec> := [<triple>...]
  <type> = (type <triples-spec>)
  "
  [triples-spec]
  (or (::triples-format (meta triples-spec))
      (let [conform (spec/conform ::triples-format triples-spec)]
        (if (= conform ::spec/invalid)
          (throw (ex-info "Invalid triples format"
                          (spec/explain-data ::triples-format triples-spec)))
          ;; else we're good
          (let [[format value] conform]
            format)))))

(spec/fdef triples-format
  :ret #{:vector-of-vectors
         :vector
         :normal-form
         })


(defmulti add-to-graph
  "Returns <g>, with <to-add> added
  Where
  <g> is a Graph
  <to-add> is interpetable as a set of triples
  Dispatched according to `triples-format`
  "
  (fn [g to-add] [(type g) (triples-format to-add)]))

;; To subtract from a graph, we can use normal form or
;; leave out  p and o ...
(spec/def ::underspecified-triple (spec/and vector?
                                            #(> (count %) 0)
                                            #(< (count %) 3)
                                            #(not (vector? (% 0)))))
(spec/def ::vector-of-underspecified (spec/and vector?
                                               #(> (count %) 0)
                                               #(vector? (% 0))))
(spec/def ::removal-format (spec/or
                            :triples-format ::triples-format
                            :underspecified-triple ::underspecified-triple
                            :vector-of-vectors ::vector-of-underspecified))

(defn triples-removal-format
  "Returns a keyword describing the format of `triples-spec` for removing a
  set of triples from a graph.
  "
  [triples-spec]
  (or (::triples-format (meta triples-spec))
      (let [conform (spec/conform ::removal-format triples-spec)]
        (if (= conform ::spec/invalid)
          (throw (ex-info "Invalid triples format"
                          (spec/explain-data ::removal-format triples-spec)))
          ;; else we're good
          (let [[format value] conform]
            (if (= format :triples-format)
              ;; value is the kind of triples format
              (let [[triples-format _] value]
                triples-format)
              ;;else :underspecifed
              format))))))

(spec/fdef triples-removal-format
  :ret #{:vector-of-vectors
         :vector
         :normal-form
         :underspecified-triple})

(defmulti remove-from-graph
  "Returns <g>, with <to-remove> removed
  Where
  <g> is a Graph
  <to-add> is interpetable as a set of triples
  Dispatched according to `triples-removal-format`
  "  
  (fn [g to-remove] [(type g) (triples-removal-format to-remove)]))

;;;;;;;;;;;;;;;
;;; Traversal
;;;;;;;;;;;;;;;;
(defn traverse 
  "Returns `acc` acquired by applying `traversal` to `g` starting with `queue`, informed by `context`
  Where
  <acc> is an arbitrary clojure 'accumulator' object (similar to a
    reduce function)
  <traversal> := (fn [g context acc queue]...)
                  -> [<context'> <acc'> <queue'>]
  <g> is a graph
  <context> := {<context-key> <context-value>....}, expressing important
    aspects of the traversal state
  <queue> := [<node> ...], nodes to visit
  <context-key> := #{:history ... maybe :skip? ... :seek ... or other
    keys specific to <traversal>, which <traversal> may use to communicate
    with future iterations of itself.
  <history> := #{<visited-node> ...}, this is conj'd with each visited node on
    each call to avoid cycles.
  <skip?> (optional) := (fn [<node>] -> true if we should skip). This may also
    be a set of nodes to skip. This allows for overriding the default skipping
    behavior which simply skips <history>
  <seek> (optional) := (fn [context acc] -> <acc'>, a function to be called
    at the start of each traversal, a truthy, non-empty response to which will
    be the immediate return value of the traverse function. This would save you
    the time and trouble of processing the whole queue, or making each traversal
    function smart enough to stop early. Must return the same type as <acc>.
  <node> is typically an element in <g>, but can be any value the traversal
    function knows how to handle
  <visited-node> is a node visited upstream. We filter these out to
    avoid cycles. This can also be specified in advance by the user.
  <target> is a node we may be searching for.
  Note: it is good practice to assign a :transition-fn metadata tag to
    transition functions, though such data is not referenced anywhere
    at this point.
"
  ([g traversal acc queue]
   (traverse g traversal {:history #{}} acc queue))
  ([g traversal context acc queue]
   {:pre [(satisfies? IGraph g)
          (fn? traversal)
          (map? context)
          (or (set? (:history context))
              (nil? (:history context)))
          (or (nil? (:seek context))
              (fn? (:seek context)))
          (or (nil? (:skip? context))
              (fn? (:skip? context))
              (set? (:skip? context)))
          (sequential? queue)
          ]
    ;; :post (= (type acc) (type %)) doesn't like recur
    }
   (let [seek (and (:seek context) ((:seek context) context acc))
         check-result (fn [result] ;; must be same type as acc
                        (assert (= (type result) (type acc)))
                        result)
         ]
     (if (and seek (not (empty? seek)))
       (check-result seek)
       ;; else no seek
       (if (or (nil? queue)
               (empty? queue))
         ;; nothing more to visit...
         (if (:seek context)
           (check-result ((:seek context) context acc))
           acc)
         ;; else the queue is not empty...
         (let [skip? (or (:skip? context)
                         (:history context)
                         #{}
                         )]
           (if (skip? (first queue))
             (recur g traversal context acc (rest queue))
             ;;else we don't skip the head of the queue...
             (let [[context acc queue-next] (traversal g context acc queue)]
               (recur g
                      traversal
                      (update context :history
                              (fn [history]
                                (conj (or history #{})
                                      (first queue))))
                      acc
                      queue-next
                      )))))))))

(defn transitive-closure 
  "Returns <traversal> for chains of `p`.
  Where
  <traversal> := (fn [g acc queue]...) -> [<context> <acc'> <queue'>], 
    s.t. <queue'> conj's all <o> s.t. (g <s> <p> <o>).  
    A traversal function argument for the `traverse` function .
  <p> is a predicate, typcially an element of <g>
  <g> is a graph.
  NOTE:
  cf the '*' operator in SPARQL property paths
  "
  [p]
  #_{:pre [(not (fn? p))] ;; direct matches only, no traversals
     ;; I think that can be relaxed now
   }
  (fn transistive-closure-traversal [g context acc queue]
    [context,
     (conj acc (first queue)),
     (reduce conj (rest queue) (g (first queue) p))]))

(defn traverse-link
  "Returns traversal function (fn [g context, acc queue]...)
    -> [context, acc', queue'], following one <p> in <g>
  Where
  <acc> is a set
  <queue> := [<node> ...], nodes to visit in traversal
  <p> is a predicate in <g>
  <g> is a graph

  NOTE: typically used as one component in a traversal path
"
  [p]
  {:pre [(not (fn? p)) ;; direct matches only. No traversals
         ]
   }
  (fn link-traversal [g context acc queue]
    (let [s (first queue)]
      [context,
       (reduce conj 
               acc
               (g s p)),
       (rest queue)])))

(defn maybe-traverse-link [p]
  "Returns traversal function (fn [g context, acc queue]...)
    -> [context, acc', queue'], 
  Where
  <acc'> includes <node> and and as many <o>s as are linked from <node>
     by <p> in <g> 
  <queue> := [<node> ...], nodes to visit in traversal
  <p> is a predicate in <g>
  <g> is a graph

  NOTE: typically used as one component in a traversal path. 
  cf the '?' operator in SPARQL property paths
"
  (fn optional-link-traversal [g context acc queue]
    (let [s (first queue)]
      [context,
       (reduce conj 
               (conj acc s)
               (g s p)),
       (rest queue)])))

(defn traverse-or [& ps]
  "Returns traversal function (fn [g context, acc queue]...)
    -> [context, acc', queue'], for `ps`
  Where
  <acc'> includes <node> and and as many <o>s as are linked from <node>
     by <p1> | <p2> | ...  in <g> 
  <queue> := [<node> ...], nodes to visit in traversal
  <ps> := [<p1>, <p2>, ...]
  <p1>, <p2>, ...  are all predicates in <g>, or traversal functions
  <g> is a graph

  cf the '|' operator in SPARQL property paths
"
  (fn traversal-disjunction [g context acc queue]
    (letfn [(collect-traversals
              [s sacc p]
              (reduce conj sacc (g s p)))
             ]
    [context,
     (reduce conj acc (reduce (partial collect-traversals (first queue))
                              #{}
                              ps)),
     (rest queue)
     ]) ))


(defn t-comp [comp-spec]
  "Returns a traversal function composed of elements specified in `comp-spec`
Where
<comp-spec> := {:path [<spec-element>, ...]
                <spec-element> {:fn <traversal-fn>
                                :doc <docstring>
                                :into <initial-acc> (default [])
                                :local-context-fn <local-fn> (default nil)
                                :update-global-context <global-fn> ( default nil)
                               }
                }
<spec-element> is typically a keyword naming a stage in the traversal, though
  it can also be a direct reference to a traversal function, in which case
  it will be equivalent to {:fn <spec-element>}
<traversal-fn-generator> := (fn [spec-element]...) -> <traversal-fn>, to be 
  invoked in cases where there is no <spec-element> in <comp-spec>,
  traverse-link is the typical choice here.
<traversal-fn> := (fn [g context acc queue]...) -> [context' acc' queue']
<context> is a traversal context conforming to the traverse function (see docs)
<update-fn> := (fn [global-context local-context] ...) -> global-context' 
  This is provided in case there is some coordination that needs to be provided
  between stages in a composed traversal.
<initial-acc> is the (usually empty) container used to initial the acc
  of the traversal stage being specified
<local-context-fn> := [global-context] -> <local-context>
<update-global-context> := [global-context local-context] -> <global-context>'
<local-context> is the context for a given stage of the traversal
<global-context> carries over between traversal stages.
Examples 
(def comp-spec  {
                    :isa? {:fn (maybe-traverse-link :isa)
                          :doc `traverses an isa link, if it exists`
                          :local-context {:doc `traversing an isa link`}
                          :update-global-context 
                          (fn [gc lc] (assoc gc 
                                             :status :followed-isa-link))
                         }
                    :subClassOf* {:fn (transitive-closure :subClassOf)
                                  :doc 'traverses 0 or more subClassof links'
                                  :local-context-fn (fn [c] {:doc 'traversing subClassOf*'})
                                  :into #{}
                                  :update-global-context
                                  (fn [gc lc] (assoc gc 
                                               :status :followed-subclassof))
                                  }
                     }})
(traversal-comp (merge comp-spec
                      {:path [:isa? :subClassOf*]
                       :doc 'Traverses the chain of subsumption links for an instance or class'
                      }))

(t-comp (merge comp-spec {:path [:isa :label] :doc 'gets class labels')))
  
Short form example:

(t-comp [:family/parent :family/brother])
... Equal to (t-comp [(traverse-link :family/parent) 
                               (traverse-link :family/brother)]
An inferred 'uncle' relation.

"
  {:pre [(or (not (:path comp-spec)) (vector? (:path comp-spec)))
         (doseq [path-spec (:path comp-spec)]
           (assert (comp-spec path-spec)))
         ] ;; TODO use clojure.spec
   }
  (let [comp-spec (if (vector? comp-spec) ;; short form, convert to long form
                    {:path comp-spec}
                    comp-spec) ;; else already in long form
        ]
    (fn composed-traversal [g context acc queue]
      {:pre [(satisfies? IGraph g)
             (map? context)
             (sequential? queue)
             ]
       }
      (loop [c context
             path (:path comp-spec) ;; [<p>, ...]
             q queue]
        (if (empty? path)
          ;; q is the final result...
          [(if-let [update-context (:update-global-context c)]
             (update-context context c)
             context)
           (into acc q)
           []]
          ;; else there's more path
          (let [p (first path)
                p-spec (or (comp-spec p) {})
                c (if-let [lcfn (:local-context-fn p-spec)]
                    (lcfn c)
                    {})
                f (cond
                    (fn? p) p
                    :default
                    (or (:fn p-spec)
                        (traverse-link p)
                        ))
                _ (assert f)
                a (or (:into (comp-spec p)) []) ;; breadth-first by default
                ]
            (recur c
                   (rest path)
                   (traverse g f c a q))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATCH-OR-TRAVERSE INVOCATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- match-or-traverse-tag [p]
  "Returns :traverse if <p> is a function, else :match
Informs p-dispatcher
"
  (if (fn? p)
    :traverse
    ;;else it's a straight match
    :match))

(defn p-dispatcher
  "Returns :traverse or :match, as a basis for dispatching standard `invoke` methods involving a <p> argument, which may be either a value ot match or a traversal function.
  "
  ([g s p]
   (match-or-traverse-tag p))
  ([g s p o]
   (match-or-traverse-tag p)))

(defmulti match-or-traverse
  "Returns values appropriate for (g s p) or (g s p o) invocations
  Where
  <o> is an object in <g>
  <s> is a subject in <g>
  <p> is either a predicate in <g> or a traversal function accumulating
    a set, starting with an empty accumulator and  queue of [<s>]
    (see docs for `traverse`)
  NOTE: Implementers of IGraph will typically use this
    method for IFn `invoke` members involving a <p> argument.
  "
  p-dispatcher)


(defmethod match-or-traverse :traverse
  ([g s p]
   {:doc "<p> is a traversal function. Aggregate a set."
    :pre (fn? p)
    }
   (traverse g p #{} [s]))
  ;;;;;;;;;;
  ([g s p o]
   {:doc "<p> is a traversal function. Stop when you find <o>."
    :pre (fn? p)
    }
   (declare unique)
   (let [seek-o (fn seek-o [context acc]
                  (clojure.set/intersection acc #{o}))
         ]
         (unique (traverse g p {:seek seek-o} #{} [s])))))

(defmethod match-or-traverse :match
  ([g s p]
   {:doc "<p> is a graph property to be matched"}
   (get-o g s p))
  ;;;;;;;;;;;;
  ([g s p o]
   (ask g s p o)))


;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;

;; Stuff to deal with cardinality one ....
(defn unique
  "Returns the single member of <coll>, or nil if <coll> is empty. Calls <on-ambiguity> if there is more than one member (default is to throw an Exception).
  Where
  <coll> is a collection
  <on-ambiguity> := (fn [coll] ...) -> <value>, default raises an error.
  Note: this can be used when you've called (G s p) and you're sure there is
    only one object.
  "
  ([coll on-ambiguity]
   (if (not (empty? coll))
     (if (> (count coll) 1)
       (on-ambiguity coll)
       (first coll))))
  ([coll]
   (unique coll (fn [coll]
                  (let [error-msg (str "Non-unique: " coll)]
                    (throw (ex-info "Unique called on non-unique collection"
                                    {:type ::NonUnique
                                     :coll coll
                                     })))))))

^{:inverse-of normalize-flat-description}
(defn flatten-description [p-o]
  "Returns `p-o` description with singletons broken out into scalars
Where
<p-o> := {<p> #{<o>}, ...}, normal form at 'description' level of a graph.
"
  (let [maybe-flatten (fn [acc k v]
                            (assoc acc
                                   k
                                   (if (and (set? v) (= (count v) 1))
                                     (unique v)
                                     v)))
        ]
    (reduce-kv maybe-flatten {} p-o)))

^{:inverse-of flatten-description}
(defn normalize-flat-description
  "Returns a normalized p-o description of `m`
  Where
  <m> is a plain clojure map"
  [m]
  (let [maybe-setify (fn [acc k v]
                        (assoc acc
                               k
                               (if (not (set? v))
                                 #{v}
                                 v)))
        ]
    (reduce-kv maybe-setify {} m)))

(defn assert-unique [g s p o]
  "Returns `g`', replacing any existing [s p *] with [s p o]"
  (add (subtract g [s p])
       [s p o]))

(defn reduce-spo [f acc g]
  "Returns <acc'> s.t. (f acc s p o) -> <acc'> for every triple in <g>
Where
<f> := (fn [acc s p o] -> <acc'>
<acc> is any value, a reduction accumlator
<s> <p> <o> constitute a triple in <g>
<g> implements IGraph
NOTE: C.f. reduce-kv
"
  ;;TODO f should conform to some spec
  (letfn [(collect-o [s p acc o]
            (f acc s p o)
            )
          (collect-p-o [s acc p]
            (reduce (partial collect-o s p)
                    acc
                    (g s p)))
          (collect-s-p-o [acc s]
            (reduce (partial collect-p-o s)
                    acc
                    (keys (g s))))
          ]
    (reduce collect-s-p-o
            acc
            (subjects g))))


