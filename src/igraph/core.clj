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
    "Returns #{<binding> ...} for query <q> applied to <g>
Where
<g> is a graph
<query> is a query in a format suitable for querying <g>
<binding> := {<var> <value>, ...}
<var> is a variable specified in <q>
<value> is a value found in <g> bounded to <var> per <q>
"
    )
  ;; for IFn
  (invoke [g] [g s] [g s p] [g s p o]
    "Applies <g> as a function to the rest of its arguments, representing 
   triples [<s> <p> <o>] in <g> respectively.
(g) -> {<s> {<p> #{<o>...}...}...} ;; = (normal-form <g>)
(g s) -> {<p> #{<o>...}, ...} ;; = (get-p-o <g>)
(g s p) -> #{<o> ...} ;; = (get-o <g>)
(g s p o) -> <o> iff [<s> <p> <o>] is in <g> ;; = (ask <g> <s> <p> <o>)
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

(defn normal-form? [m]
  "Returns true iff <m> is in normal form for IGraph."
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


;;;;;;;;;;;;;;;
;;; TRAVERSAL
;;;;;;;;;;;;;;;;

(defn traverse 
  "Returns `acc` acquired by applying `traversal` to `g`starting with `to-visit`, skipping members in `history`.
  Where
  <acc> is an arbitrary clojure object
  <traversal> := (fn [g acc to-visit]...) -> [<acc'> <to-visit'>]
  <g> is a graph
  <to-visit> := [<node> ...]
  <history> := #{<visited node> ...}, this is conj'd with each call.
  <node> is typically an element in <g>
  <visited-node> is a node visited upstream. We filter these out to
    avoid cycles.
"
  ([g traversal acc to-visit]
   (traverse g traversal acc to-visit #{}))
  
  ([g traversal acc to-visit history]
   (if (or (nil? to-visit)
           (empty? to-visit))
     ;; nothing more to visit...
     acc
     ;; else we keep going
     (if (history (first to-visit))
       (recur g traversal acc (rest to-visit) history)
       ;;else we're not in a cycle
       (let [[acc to-visit-next] (traversal g acc to-visit)]
         (recur g
                traversal
                acc
                to-visit-next
                (conj history (first to-visit))))))))

(defn transitive-closure [p]
  "Returns <traversal> for chains of `p`.
Where
<traversal> := (fn [g acc to-visit]...) -> [<acc'> <to-visit'>], 
  s.t. <to-visit'> conj's all <o> s.t. (g <s> <p> <o>).  
  A traversal function argument for the `traverse` function .
<p> is a predicate, typcially an element of <g>
<g> is a graph.
"
  (fn [g acc to-visit]
    [(conj acc (first to-visit))
     (concat to-visit (g (first to-visit) p))]))
