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
- `(normal-form g)` -> {s {p #{o...}...}...}
- `(add g to-add)` -> new graph with <to-add> added
- `(get-p-o g s)` -> {p #{o...} ...}
- `(get-o g s p)` -> #{o ...}
- `(ask g s p o)` ->  o
- `(query g q)` -> [{<var> <value> ...} ...]

Also `invoke` to support `IFn` as follows
- `(g)` = `(normal-form g)`
- `(g s)` = `(get-p-o g s)`
- `(g s p)` = `(get-o g s p)`
- `(g s p o)` = `(ask g s p o)`

The [source file]([IGraph](https://github.com/ont-app/igraph/blob/master/src/igraph/core.clj)) has fairly explicit docstrings.

## Graph

The Graph type is a very lightweight implementation supporting IGraph. The aim here aside from demonstrating IGraph is to add just one layer of expressiveness to `map`.

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
   [:mary :isa :person]
   [:likes :isa :property]
   [:isa :isa :property]
   [:john :likes :meat]
   [:mary :likes :coke]
   [:meat :isa :food]
   [:coke :isa :drink]
   [:mary :name {:value "Mary" :lang "en"}]
   [:john :name {:value "John" :lang "en"}]
  ])))
->
#object[igraph.graph.Graph 0x58b96f62 "igraph.graph.Graph@58b96f62"]
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

Invoked with subject predicate and object gives you the object (if its there):
```
(g :john :likes :meat)
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
