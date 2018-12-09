# igraph

IGraph defines a protocol which aims to capture some generality amongst a 
plurality of graph-based representations (RDF, datascript, datomic....)

There is also a type Graph defined which implements IGraph.


## Installation

This is deployed to [clojars](https://clojars.org/ont-app/igraph).

Depedency declaration for leiningen:

```
[ont-app/igraph "0.1.0-SNAPSHOT"]
```

## Usage

### IGraph
The `IGraph` protocol specifies the following functions:
#### member access
- `(normal-form g)` -> {s {p #{o...}...}...}
- `(subjects g)` -> (s ...), a seq of subjects
- `(get-p-o g s)` -> {p #{o...} ...}
- `(get-o g s p)` -> #{o ...}
- `(ask g s p o)` ->  o
- `(query g q)` -> [{var value ...} ...]
#### membership changes
- `(read-only? g) -> true if changing membership throws an exception
- `(add g to-add)` -> new graph with <to-add> added
- `(subtract g to-subtract)` -> new graph with <to-subtract> absent.

Also `invoke` to support `IFn` as follows
- `(g)` = `(normal-form g)`
- `(g s)` = `(get-p-o g s)`
- `(g s p)` = `(get-o g s p)`
- `(g s p o)` = `(ask g s p o)`

The [source file]([IGraph](https://github.com/ont-app/igraph/blob/master/src/igraph/core.clj)) has fairly explicit docstrings.

## Graph

The Graph type is a very lightweight implementation of IGraph. The aim here, aside from demonstrating IGraph, is to add just one layer of expressiveness to `map`.

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

One subracts from it like this:

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


See also the  [test file](https://github.com/ont-app/igraph/blob/master/test/igraph/graph_test.clj).

### Bugs

Probably lots. This is brand-spankin' new.


## License

Copyright © 2018 Eric D. Scott

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
