# igraph

IGraph defines a protocol which aims to capture some generality amongst a 
plurality of graph-based representations (RDF, datascript, datomic....)

There is also a type Graph defined which implements IGraph.


## Installation

This is deployed to [clojars](https://clojars.org/ont-app/igraph):

[![Clojars Project](https://img.shields.io/clojars/v/ont-app/igraph.svg)](https://clojars.org/ont-app/igraph)

## Usage

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

Also `invoke` to support `IFn` as follows
- `(g)` = `(normal-form g)`
- `(g s)` = `(get-p-o g s)`
- `(g s p)` = `(get-o g s p)`
- `(g s p o)` = `(ask g s p o)`


#### Traversal

- `(traverse g traversal acc to-visit)` -> acc, traversing `g` per `traversal`, starting with `to-visit`.
- `(trasitive-closure p) -> (fn [g acc to-visit] ...) -> [acc' to visit'], a traversal argument to `traverse`.

See the example in the `Graph` section below.

#### utilities
- `(normal-form? m)` -> true iff m is a map in normal form.


The [source file](https://github.com/ont-app/igraph/blob/master/src/igraph/core.clj) has fairly explicit docstrings.

### ISet

It may make sense for some implementations of IGraph also to implment the basic set operations, defined in ISet:

- `(union g1 12)` -> A new graph with all triples from both graphs
- `(difference g1 g2)` -> A new graph with triples in g1 not also in g2
- `(intersection g1 g2)` -> A new graph with only triples shared in both graphs

## Graph

The Graph type is a very lightweight implementation of IGraph. The aim here, aside from demonstrating IGraph, is to add just one layer of expressiveness over the  `map` construct.

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
  ])))
->
#object[igraph.graph.Graph 0x58b96f62 "igraph.graph.Graph@58b96f62"]
```

The `subjects` function will give you the subjects:
```
(subjects my-graph)
-> 
(:john :mary :likes :isa :meat :coke)
```


Invoked without arguments gives you `normal form`:

```
(my-graph)
->
{:john
 {:isa #{:person},
  :likes #{:meat},
  :name #{{:value "John", :lang "en"}}},
 :mary
 {:isa #{:person},
  :likes #{:coke},
  :name #{{:value "Mary", :lang "en"}}},
 :likes {:isa #{:property}},
 :isa {:isa #{:property}},
 :meat {:isa #{:food}},
 :coke {:isa #{:drink}}}
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

Querying is done with a very simple graph pattern using keywords starting with ?:
```
(query my-graph
    [[:?liker :likes :?likee]
     [:?likee :isa :?type]])
-> 
#{{:?type :drink, :?likee :coke, :?liker :mary}
  {:?type :food, :?likee :meat, :?liker :john}}
```

Traversal is done with a function that returns the accumulator and a possibly empty list of nodes in the graph still to visit...
```
(def g (add (make-graph) [[:a :isa :b] 
                          [:b :isa :c]
                          [:c :isa :d]]))

(defn isa* [g acc to-visit]
   [(conj acc (first to-visit))
    (concat to-visit (g (first to-visit) :isa))])
   
(traverse g isa* [] [:a])
->
[:a :b :c :d]
```

The isa* function defined above is equivalent to `transitive-closure`:
```
(traverse g (transitive-closure :isa) [] [:a])
->
[:a :b :c :d]
```

One subtracts from it like this (also returns new immutable object):

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

Graph also implements the `ISet` functions `union`, `difference` and `intersection`.

See also the  [test file](https://github.com/ont-app/igraph/blob/master/test/igraph/graph_test.clj).

### Bugs

Probably lots. This is brand-spankin' new.


## License

Copyright © 2018 Eric D. Scott

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
