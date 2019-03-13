(ns ^{:author "Eric D. Scott",
      :doc "Abstractions over a graph object, intended to sit alongside the 
other basic clojure data structures such as maps and sequences.
"}
    igraph.core
  (:require [clojure.set :as set]
            ))

           
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CONTENT MANIPULATION FUNCTIONS
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (read-only? [g]
    "Returns true if the membership of <g> is static 
and add/subtract functions will throw an exception. This 
may hold for example when <g> is a public endpoint for which write 
permission is denied"
    )
  (add [g to-add]
    "Returns <g>, with <to-add> added to its contents.
Throws an exception if (read-only? <g>)
Where
<g> is a graph
<to-add> is in some format interpretable as a set of triples.
"
    )
  (subtract [g to-subtract]
    "Returns <g> with <to-subtract> removed from its contents.
Throws an exception if (read-only? <g>)
Where
<g> is a graph
<to-subtract> is in some format interpretable as a set of triples.
"
    )
  )

(defn normal-form? 
  "Returns true iff <m> is in normal form for IGraph."
  [m]
  (and (instance? clojure.lang.APersistentMap m)
       (or (empty? m)
           (let [p (m (first (keys m)))
                 o (p (first (keys p)))
                 ]
             (instance? clojure.lang.APersistentSet o)))))

(defprotocol ISet
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

(defn alter-graph-dispatcher 
  "Returns one of #{:vector :vector-of-vectors :normal-form <type>} for <args>
  Where
  <args> := [<g> <to-add-or-remove>],  arguments to a method add or remove from graph
  <g> is a graph
  <to-add-or-remove> is a specification of triples to add to or remove from  <g>
  <triple> indicates <to-add-or-remove> := [<s> <p> <o>]
  <vector-of-vectors> indicates <to-add-or-remove> := [<triple>...]
  <type> = (type <to-add>)
  "
  [g to-add-or-remove]
  (if (= (type to-add-or-remove) clojure.lang.PersistentVector)
    (if (and (> (count to-add-or-remove) 0)
             (= (type (to-add-or-remove 0)) clojure.lang.PersistentVector))
      :vector-of-vectors
      :vector)
    ;; else not a vector
    (if (normal-form? to-add-or-remove)
      :normal-form
      ;; else neither a vector nor normal form
      (type to-add-or-remove))))

(defmulti add-to-graph
  "Returns <g>, with <to-add> added
  Where
  <g> is a Graph
  <to-add> is interpetable as a set of triples
  Dispatched according to `alter-graph-dispatcher`
  "
  alter-graph-dispatcher)

(defmulti remove-from-graph
  "Returns <g>, with <to-add> added
  Where
  <g> is a Graph
  <to-add> is interpetable as a set of triples
  Dispatched according to `alter-graph-dispatcher`
  "  
  alter-graph-dispatcher)

;;;;;;;;;;;;;;;
;;; TRAVERSAL
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
   {:pre [(fn? traversal)
          (map? context)
          (or (set? (:history context))
              (nil? (:history context)))
          (or (nil? (:seek context))
              (fn? (:seek context)))
          (or (nil? (:skip? context))
              (fn? (:skip? context))
              (set? (:skip? context)))
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
<traversal> := (fn [g acc queue]...) -> [<acc'> <queue'>], 
  s.t. <queue'> conj's all <o> s.t. (g <s> <p> <o>).  
  A traversal function argument for the `traverse` function .
<p> is a predicate, typcially an element of <g>
<g> is a graph.
  "
  [p]
  {:pre [(not (fn? p))] ;; direct matches only, no traversals
   }
  ^:traversal-fn
  (fn transitive-closure-traversal [g context acc queue]
    [context,
     (conj acc (first queue)),
     (concat (rest queue) (g (first queue) p))]))




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
  ^:traversal-fn
  (fn link-traversal [g context acc queue]
    (let [s (first queue)]
      [context,
       (reduce (fn [acc node]
                 (conj acc node))
               acc
               (g s p)),
       (rest queue)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATCH OR TRAVERSE INVOCATION
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
   (unique coll (fn [coll] (throw (Exception. (str "Non-unique: " coll)))))))



