# igraph

IGraph defines a protocol which aims to capture some generality amongst a 
plurality of graph-based representations (RDF, datascript, datomic....)

There is also a type Graph defined which implements IGraph.


## Installation

This is deployed to [clojars](https://clojars.org/ont-app/igraph):

[![Clojars Project](https://img.shields.io/clojars/v/ont-app/igraph.svg)](https://clojars.org/ont-app/igraph)

### IGraph
The `IGraph` protocol specifies the following functions:
#### Member access
- `(normal-form g)` -> {s {p #{o...}...}...}
- `(subjects g)` -> (s ...), a collection of subjects
- `(get-p-o g s)` -> {p #{o...} ...}
- `(get-o g s p)` -> #{o ...}
- `(ask g s p o)` ->  truthy 
- `(query g q)` -> collection of {var value ...} maps
#### Membership changes
- `(read-only? g)` -> true if changing membership throws an exception
- `(add g to-add)` -> new graph with <i>to-add</i> present
- `(subtract g to-subtract)` -> new graph with <i>to-subtract</i> absent

Also a corresponding `invoke` to support `IFn` as follows
- `(g)` = `(normal-form g)`
- `(g s)` -> {p #{o...} ...}
- `(g s p)` -> #{o ...}
- `(g s p o)` ->  truthy 


#### Traversal

There is a traversal regime based on a calling function called `traverse`:

- `(traverse g traversal context acc queue)` -> `acc`

    ... traversing `g` per the `traversal` function, starting with the
  first element of `queue`, possibly informed by `context`.

This function will repeatedly call the `traversal` function until `queue` is empty, returning the final value for `acc`.

The `context` argument is a map containing key-values which may inform the course of the traversal. These may include:

- `:history` this will be set by `traverse`, and updated to hold all
  elements encountered in the course of the traversal. In order to
  avoid cycles, any element in the history will be skipped should it
  ever re-appear at the head of the queue.
- `:skip?` (optional), a function (fn[x] ...) -> truthy (or a set)
  which will override `:history`.
- `:seek` (optional), a function (fn [context acc]...) -> <acc'>. If
  specified, this function will be called at the beginning of each
  traversal, and if truthy and non-empty the traversal will end
  immediately with that value.


##### Traversal functions
A `traversal function` has the signature (fn [g context acc queue]...) -> [context' acc' queue'].

The `context` argument is a map that acts as a blackboard reflecting the global state of the traversal. This might for example be data that guides some kind of beam-search. It should only hold values that become irrelevant after the traversal has completed.

###### Transitive closure

- `(trasitive-closure p)` -> (fn [g context acc to-visit] ...) -> [context' acc' queue'], 
  
  This returns a traversal function which starting with a `queue = [s]` will accumulate all `o` s.t. `s` is associated with `o` through zero or more `p` links.

###### As the `p` argument in accessors

Recall that implementations of IGraph should provide `invoke` functions with 0-3 arguments.

Two of these functions involve specification of a `p` parameter:

```
(g s p) -> {<o>...}
(g s p o) -> truthy.
```

`(match-or-traverse g s p)` -> #{<o>...}
`(match-or-traverse g s p o)` -> truthy

The `p` argument is typically the identifier of a graph element in `g`, but it can also optionally be a traversal function which starts at `s` and accumulates a set of `o`s.

See also the subclassOf* example in the discussion below describing the of the `Graph` type.

#### Multimethods to add/remove from graph
There are multi-methods defined `add-to-graph` and `remove-from-graph`, dispatched on `alter-graph-dispatcher`

```
(alter-graph-dispatcher g to-add-or-remove)
-> 
One of :normal-form, :vector, :vector-of-vectors, or the type of `to-add-or-remove`
```

Implementations of IGraph will typically define methods for each of
these values when defining `add` and `subtract`.


#### utilities
- `(normal-form? m)` -> true iff m is a map in normal form.


### ISet

It may make sense for some implementations of IGraph also to implement
the basic set operations, defined in ISet:

- `(union g1 12)` -> A new graph with all triples from both graphs
- `(difference g1 g2)` -> A new graph with triples in g1 not also in g2
- `(intersection g1 g2)` -> A new graph with only triples shared in both graphs

## The `Graph` type

The Graph type is a very lightweight implementation of IGraph. The aim
here, aside from demonstrating IGraph, is to add just one layer of
expressiveness over the `map` construct.

To create:

```
(make-graph)
-> 
#object[igraph.graph.Graph 0x67e46c69 "igraph.graph.Graph@67e46c69"]
```

One adds to it like this (returns a new immutable object):

```
(add my-graph
  [[:john :isa :person]
   [:john :likes :meat]
   [:john :name {:value "John" :lang "en"}]
   [:mary
    :isa :person
    :likes :coke
    :name {:value "Mary" :lang "en"}
    ]
   [:likes :isa :property]
   [:isa :isa :property]
   [:meat :isa :food]
   [:coke :isa :drink]
   [:drink :subClassOf :consumable]
   [:food :subClassOf :consumable]
   [:consumable :subClassOf :thing]
   [:person :subClassOf :thing]
  ])))
->
#object[igraph.graph.Graph 0x58b96f62 "igraph.graph.Graph@58b96f62"]
```

The `subjects` function will give you the subjects:
```
(subjects my-graph)
-> 
(:john :mary :likes :isa :meat :coke :drink :food :consumable :person)
```

Invoked without arguments gives you `normal form`:

```
(pprint (my-graph))
->
{:consumable {:subClassOf #{:thing}},
 :person {:subClassOf #{:thing}},
 :isa {:isa #{:property}},
 :drink {:subClassOf #{:consumable}},
 :likes {:isa #{:property}},
 :coke {:isa #{:drink}},
 :meat {:isa #{:food}},
 :food {:subClassOf #{:consumable}},
 :john
 {:isa #{:person},
  :likes #{:meat},
  :name #{{:value "John", :lang "en"}}},
 :mary
 {:isa #{:person},
  :likes #{:coke},
  :name #{{:value "Mary", :lang "en"}}}}

```
Invoked with a subject gives you its predicate-object map:
```
(my-graph :john)
->
{:isa #{:person}, 
 :likes #{:meat}, 
 :name #{{:value "John", :lang "en"}}}
```

Invoked with a subject and predicate gives you the set of objects:
```
(my-graph :john :likes)
->
#{:meat}
```

Traversal is done with a function that returns the accumulator and a possibly empty list of nodes in the graph still to visit...

```
(defn subClassOf* [g context acc to-visit]
   [context,
    (conj acc (first to-visit)),
    (concat (rest to-visit) (g (first to-visit) :isa))])
   
(traverse g subClassOf* [] [:drink])
->
[:drink :consumable :thing]
```

The subClassOf* function defined above is equivalent to `transitive-closure`:
```
(traverse g (transitive-closure :subClassOf) [] [:a])
->
[:drink :consumable :thing]
```

A graph can be invoked with a subject and a traversal function as its
`p` argument, which will give you the result of the traversal with a
starting queue of [s] (as a set):

```
(def subClassOf* (transitive-closure :subClassOf))

(my-graph :drink subClassOf*)
-> 
#{:consumable :drink :thing}

```


If you're sure there's only going to be one object you can use the `unique` function:
```
(unique (my-graph :john :likes))
->
:meat

(unique #{:just-me :no-theres-me-too!})
-> 
Exception Non-unique: #{:no-theres-me-too! :just-me}

```

Invoked with subject, predicate and object gives you the object (if its there):
```
(my-graph :john :likes :meat)
->
:meat
```

Or with with a traversal function:

```
(my-graph :drink subClassOf* :thing)
;; ->
:thing
```

Querying is done with a very simple graph pattern using keywords starting with ?:
```
(query my-graph
    [[:?liker :likes :?likee]
     [:?likee :isa :?type]])
-> 
#{{:?type :drink, :?likee :coke, :?liker :mary}
  {:?type :food, :?likee :meat, :?liker :john}}
```

We can also use traversal functions for the p argument:
```
(query my-graph
  [[:?liker :likes :?likee]
   [:?likee :isa :?class]
   [:?class subClassOf* :?super]])
->
#{{:?super :thing, :?class :food, :?likee :meat, :?liker :john}
  {:?super :thing, :?class :drink, :?likee :coke, :?liker :mary}
  {:?super :food, :?class :food, :?likee :meat, :?liker :john}
  {:?super :consumable, :?class :drink, :?likee :coke, :?liker :mary}
  {:?super :consumable, :?class :food, :?likee :meat, :?liker :john}
  {:?super :drink, :?class :drink, :?likee :coke, :?liker :mary}}
```

One subtracts from a `Graph` like this (also returns new immutable object):
```
(normal-form 
  (subtract 
    (add (make-graph) 
         [[:a :b :c :d :e] [:g :h :i]])
    [:a]))
;; -> 
;; {:g {:h #{:i}}}

(normal-form 
  (subtract 
    (add (make-graph) 
         [[:a :b :c :d :e] [:g :h :i]])
    [:a :b]))
;; ->
;; {:a {:d #{:e}}, :g {:h #{:i}}}

(normal-form 
  (subtract 
    (add (make-graph) 
         [[:a :b :c :d :e] [:g :h :i]])
    [:a :b :c]))
;; ->
;; {:a {:d #{:e}}, :g {:h #{:i}}}

(normal-form 
  (subtract 
    (add (make-graph) 
         [[:a :b :c :d :e] [:g :h :i]])
    [[:a :b][:g :h :i]]))
;; ->
;; {:a {:d #{:e}}}

```

`Graph` also implements the `ISet` functions `union`, `difference` and `intersection`.


See also the  [test file](https://github.com/ont-app/igraph/blob/master/test/igraph/graph_test.clj).

### Bugs

Probably lots. This is brand-spankin' new.

## License

Copyright © 2019 Eric D. Scott

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
