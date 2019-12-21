# igraph

IGraph defines a protocol which aims to capture some generality amongst a 
plurality of graph-based representations (RDF, datascript, datomic....)

There is also a type Graph defined which implements IGraph.

## Contents
### [Motivation[(#Motivation)
### [Dependencies](#Dependencies)
### [The IGraph protocol](#The_IGraph_protocol)
#### [Methods summary](#IGraph_Methods_summary)
#### [Implementation: `ont-app.igraph.graph/Graph`](#Graph_implementation)
#### [Member access](#Member_access)
##### [Normal form](#Normal_form)
##### [Tractability](#Tractability)
##### [`subjects`](#Subjects_method)
##### [`get-p-o`](#get-p-o_method)
##### [`get-o`](#get-o_method)
##### [`ask`](#ask_method)
##### [`query`](#query_method)
##### [`invoke` for arities 0-3](#invoke_method)
#### [Content Manipulation](#Content_Manipulation)
##### [`read-only?`](#read-only_method)
##### [`triples-format`](#triples-format)
##### [`add`](#add_method)
##### [`subtract`](#subtract_method)
### [The IGraphSet protocol](#The_IGraphSet_protocol)
#### [Methods summary](#IGraphSet_methods_summary)
#### [`union`](#union_method)
#### [`intersection`](#intersection_method)
#### [`difference`](#difference_method)
### [Traversal](#Traversal)
#### [`traverse`](#traverse_method)
#### [Traversal functions](#Traversal_functions)
##### [`transitive-closure`](#transitive-closure)
#### [`traverse-link`](#traverse-link)
##### [`maybe-traverse-link`](#maybe-traverse-link)
##### [`traverse-or`](#traverse-or)
#### [Traversal composition with `t-comp`](#Traversal_composition`)
##### [short form](#t-comp__short_form)
##### [long form](#t-comp_long_form)
#### [Using traversal functions as a `p` argument to accessor functions](#traversal-fn-as-p)
### [cardinality-1 utilites](#cardinality-1_utilites)
#[### `unique`](#unique)
#### [`flatten-description`](#flatten-description)
#### [`normalize-flat-description`](#normalize-flat-description)
#### `assert-unique`
### [i/o](#i-o)
### [Other utilities](#Other_utilities)
#### [`reduce-spo`](#reduce-spo)
#### [`normal-form?`](#normal-form-q)
## [`ont-app.igraph.graph/Graph`](#Graph)
### [Querying](#Querying)

<a name="Motivation"></a>
## Motivation
<a name="Dependencies"></a>
## Dependencies
<a name="The_IGraph_protocol</a>
## The IGraph protocol
<a name="Igraph_methods_summary"></a>
### Methods summary
<a name="#Graph_implementation"></a>
### Implementation: `ont-app.igraph.graph/Graph`
<a name="Member_access"></a>
### Member access
<a name="Normal_form"></a>
#### Normal form
<a name="Tractability"></a>
#### Tractability
<a name="subjects_method"></a>
#### `subjects`
<a name="get-p-o_method"></a>
#### `get-p-o`
<a name="get-o_method-"></a>
#### `get-o`
<a name="ask_method"></a>
#### `ask`
<a name="query_method"></a>
#### `query`
<a name="invoke_method"></a>
#### `invoke` for arities 0-3
<a name="Content_Manipulation"></a>
### Content Manipulation
<a name="read-only_method"></a>
#### `read-only?`
<a name="triples-format"></a>
#### triples-format
<a name="add_method"></a>
#### `add`
<a name="subtract_method"></a>
#### `subtract`
<a name="The_IGraphSet_protocol"></a>
## The IGraphSet protocol

<a name="IGraphSet_methods_summary"></a>
### Methods summary
<a name="union_method"></a>
### `union`
<a name="intersection_method"></a>
### `intersection`
<a name="difference_method"></a>
### `difference`
<a name="Traversal"></a>
## Traversal
<a name="traverse_method"></a>
### `traverse`
<a name="Traversal_functions"></a>
### Traversal functions
<a name="transitive-closure"></a>
#### `transitive-closure`
<a name="traverse-link_method"></a>
### `traverse-link`
<a name="maybe-traverse-link"></a>
#### `maybe-traverse-link`
<a name="traverse-or"></a>
#### `traverse-or`
<a name="Traversal_composition"></a>
### Traversal composition with `t-comp`
<a name="t-comp_short_form"></a>
#### short form
<a name="t-comp_long_form"></a>
#### long form
<a name="traversal-fn-as-p"></a>
### Using traversal functions as a `p` argument to accessor functions
<a name="cardinality-1_utilities"></a>
## cardinality-1 utilites
<a name="unique"></a>
### `unique`
<a name="flatten-description"></a>
### `flatten-description`
<a name="normalize-flat-description"></a>
### `normalize-flat-description`
<a name="assert-unique"></a>
### `assert-unique`
<a name="i-o"></a>
## i/o
<a name="Other_utilities"></a>
## Other utilities
<a name="reduce-spo"></a>
### `reduce-spo`
<a name="normal-form-q"></a>
### `normal-form?`
<a name="Graph"></a>
# `ont-app.igraph.graph/Graph`
<a name="querying"></a>
## Querying
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>
<a name=""></a>



## Installation

This is deployed to [clojars](https://clojars.org/ont-app/igraph):

[![Clojars Project](https://img.shields.io/clojars/v/ont-app/igraph.svg)](https://clojars.org/ont-app/igraph)

With snapshot
```
[ont-app/igraph "0.1.4-SNAPSHOT"]
```
Require thus:
```
(:require 
  [ont-app.igraph.core] ;; for the IGraph protocol and related stuff
  [ont-app.igraph.graph] ;; for the Graph implementation of IGraph
  )
           
```

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

There is a traversal regime based on a calling function called `traverse`, which is somewhat analogous to `reduce`:

- `(traverse g traversal context acc queue)` -> `acc`

    ... traversing `g` per the `traversal` function, starting with the
  first element of `queue`, possibly informed by `context`.

This function will repeatedly call the `traversal` function until `queue` is empty, returning the final value for `acc`. Each call to the traversal function returns modified versions of `context`, `acc` and `queue`.

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

###### Utility: `transitive-closure`

- `(trasitive-closure p)` -> (fn [g context acc to-visit] ...) -> [context' acc' queue'], 
  
  This returns a traversal function which starting with a `queue = [s]` will accumulate all `o` s.t. `s` is associated with `o` through zero or more `p` links.

###### Utility: `traverse-link`

- `(traverse-link p)` -> (fn [g context acc queue] ...) -> [context acc' []],

The function returned by this call when called as a traversal will
accumulate all `o` s.t. for all `s` in `queue`, (g s p o). This is is
useful for example in specifying the first (<i>instance-of</i>) stage of a complex traversal <i>x instance-of/subclass-of* y</i>.

###### Utility: `traverse-disjunction`

- `(traverse-disjuction & ps)` -> (fn [g context acc queue] ...) -> [context acc' []],

Where `ps` is one or more predicates. Matches any of those predicates.


###### Utility: `maybe-traverse-link`

- `(maybe-traverse-link p)` -> (fn [g context acc queue] ...) -> [context acc' []]

Matches 0 or 1 occurrances of `p`.

###### As the `p` argument in accessors

Recall that implementations of IGraph should provide `invoke` functions with 0-3 arguments.

Two of these functions involve specification of a `p` parameter:

```
(g s p) -> {<o>...}

(g s p o) -> truthy.
```

This is informed by a multimethod dispatched on whether `p` is a function:

`(match-or-traverse g s p)` -> #{<o>...}
`(match-or-traverse g s p o)` -> truthy

The `p` argument is typically the identifier of a graph element in `g`, but it can also optionally be a traversal function which starts at `s` and accumulates a set of `o`s.

See also the `subClassOf*` examples in the discussion below describing the of the `Graph` type.

###### Composition of traversal functions (`traversal-comp`)

Composition functions are composable with a 'short form' and a 'long form'.

###### Short form composition

Short-form composition can be used when the traversal function meets the followin criteria:
- None of the component functions manipulate the traversal context
- Each component function accumulates a sequential value suitable to serve as the initual queue of the component function that follows it.

Such functions can be called as simple vector:

`(traversal-comp [(maybe-traverse :rdf/type) (transitive-closure :rdfs/subClassOf)])`

Is equivalent to the SPARQL property path `a?/rdfs:subClassOf*`

###### Long form composition

In cases where one wants to compose traversal function that cannot meet the criteria above, then instead of passing to `traversal-comp` in a vector of traversal functions, one passes in a map with the following keys:
```
{ :path  [:traversal-name-1 :traversal-name-2...]
   :default-fn <traversal-fn-generator>
   :traversal-name-1 {:fn <traversal-fn>
                      :doc <docstring>
                      :into <initial accumulator> (default [])
                      :local-context <context> (default {})
                      :update-global-context <update-fn> (default nil)
                      }
   traversal-name-2 ...
   ...
 }
 ```
These parameters allow you to as much control as you need over the various traversal contexts in play, and making sure that the output values from one traversal feed appropriately into the initial queue of the next traversal.

But most of the time, the short form is all that's needed. See the docstring of `traversal-comp` for more on the long form.


#### Multimethods to add/remove from graph
There are multi-methods defined `add-to-graph` and `remove-from-graph`, dispatched on `triples-format`

```
(triples-format g to-add-or-remove)
-> 
;; One of `::normal-form`, `::vector`, `::vector-of-vectors`, or defaulting to the type of `to-add-or-remove`
```

Implementations of IGraph will typically define methods for each of
these values when defining `add` and `subtract`.

`::normal-form`, `::vector` and `::vectorOfVectors` have `clojure.spec` definitions.

#### utilities
- `(normal-form? m)` -> true iff m is a map in normal form.


- `(reduce-spo f acc g) -> `acc'` s.t. `f` is called on each triple in `g`.
Where `f` := `(fn [acc s p o]...) -> acc'`

##### cardinality-1 utilites
Requiring normal form to provide a set as its 3rd-tier representation has the advantage of ensuring the the normal form is as simple as possible, and makes it easy to think about set operations over graphs, but it can be a bit unwieldy when dealing with the many cases where the discriptive map's keys reliably map to a single scalar value.

The following utilities are provided to help:

- `(unique [x]) -> x` There is an optional 2nd parameter to deal with the case where the argument is not a singleton, but by default it raises an error.
- `(flatten-description (g s))` Automatically translates the p-o description into a simple k-v mappings when only a single object exists.
- `(normalize-flat-description m)` inverts the flattened description and renders it into a form that can easily be added back into a graph.

##### I/O
`(write-to-file [path g] ...) -> path` (clj only)

Will write an edn file with the normal form contents of `g`.

`(read-from-file [g path] ...) -> g'` (clj only)

Will read the an edn the normal form contents of `path` into `g`.

These is defined for clj hosts only.

### IGraphSet

It may make sense for some implementations of IGraph also to implement
the basic set operations, defined in IGraphSet:

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
#object[ont-app.igraph.graph.Graph 0x67e46c69 "ont-app.igraph.graph.Graph@67e46c69"]
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
#object[ont-app.igraph.graph.Graph 0x58b96f62 "ont-app.igraph.graph.Graph@58b96f62"]
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
(traverse g (transitive-closure :subClassOf) [] [:drink])
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

### Future work

- `clojure.spec` will be much more in evidence
- There will be a weighted-normal-form, whose 3rd tier will map objects to numeric weights
- `igraph.graph` will have better query planning and indexing
- Datomic, loom, ubergraph, and other graph-oriented libraries will be ported 

## License

Copyright © 2019 Eric D. Scott

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
