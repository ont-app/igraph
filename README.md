<img src="http://ericdscott.com/NaturalLexiconLogo.png" alt="NaturalLexicon logo" :width=100 height=100/>

# ont-app/igraph

IGraph defines a protocol which aims to provide a general interface to
a variety of graph-based representations (RDF, datascript, datomic,
loom, ...)

It also defines a `Graph` datatype which implements `IGraph`.

## Contents
- [Dependencies](#h2-dependencies)
- [Motivation](#h2-motivation)
- [The IGraph protocol](#h2-igraph-protocol)
  - [Methods summary](#h3-methods-summary)
  - [Member access](#Member_access)
    - [Normal form](#Normal_form)
    - [Tractability](#h4-tractability)
    - [`subjects`](#subjects_method)
    - [`get-p-o`](#get-p-o_method)
    - [`get-o`](#get-o_method)
    - [`ask`](#ask_method)
    - [`query`](#query_method)
    - [`invoke` for arities 0-3](#invoke_method)
  - [Content Manipulation](#Content_Manipulation)
    - [`mutability`](#mutability_method)
    - [The `add-to-graph` multimethod](#add-to-graph)
    - [The `remove-from-graph` multimethod](#remove-from-graph)
- [The IGraphImmutable protocol](#IGraphImmutable)
    - [`add`](#add_method)
    - [`subtract`](#subtract_method)
- [The IGraphMutable protocol](#IGraphMutable)
    - [`add!`](#add!_method)
    - [`subtract!`](#subtract!_method)
- [The IGraphAccumulateOnly protocol](#IGraphAccumulateOnly)
    - [`claim`](#claim_method)
    - [`retract`](#retract_method)
- [The IGraphSet protocol](#h2-igraphset-protocol)
  - [Methods summary](#h3-igraphset-methods-summary)
  - [`union`](#union_method)
  - [`intersection`](#intersection_method)
  - [`difference`](#difference_method)
- [Traversal](#Traversal)
  - [The `traverse` function](#traverse_function)
  - [Traversal functions](#Traversal_functions)
    - [context](#h4-context)
    - [The queue](#queue)
  - [Traversal utilities](#Traversal_utilities)
    - [`transitive-closure`](#h4-transitive-closure)
    - [`traverse-link`](#h4-traverse-link)
    - [`maybe-traverse-link`](#h4-maybe-traverse-link)
    - [`traverse-or`](#h4-traverse-or)
  - [Traversal composition with `t-comp`](#Traversal_composition)
    - [short form](#h4-t-comp-short)
    - [long form](#h4-t-comp-long)
  - [Using traversal functions as a `p` argument to `invoke`](#traversal-fn-as-p)
- [cardinality-1 utilites](#cardinality-1_utilities)
  - [`unique`](#h3-unique)
  - [`flatten-description`](#h3-flatten-description)
  - [`normalize-flat-description`](#h3-normalize-flat-description)
  - [`assert-unique`](#h3-assert-unique)
- [I/O](#i-o)
  - [`write-to-file`](#h3-write-to-file)
  - [`read-from-file`](#h3-read-from-file)
- [Other utilities](#Other_utilities)
  - [`reduce-spo`](#h3-reduce-spo)
- [Implementations](#h2-implementations)
  - [`ont-app.igraph.graph/Graph`](#Graph)
    - [Graph creation](#h4-graph-creation)
    - [Querying](#h4-querying)
  - [sparql-client](#h3-sparql-client)
  - [datascript-graph](#h3-datascript-graph)
  - [datomic-client](#h3-datomic-client)
- [Future Work](#h2-future-work)
- [License](#h2-license)
---

<a name="h2-dependencies"></a>
## Dependencies

This is deployed to [clojars](https://clojars.org/ont-app/igraph):

[![Clojars
Project](https://img.shields.io/clojars/v/ont-app/igraph.svg)](https://clojars.org/ont-app/igraph)

```
[ont-app/igraph "0.1.5"]
```

Require thus:
```
(:require 
  [ont-app.igraph.core :as igraph] ;; for the IGraph protocol and related stuff
  [some.igraph.implementation ...] ;; implements IGraph
  )
           
```

<a name="h2-motivation"></a>
## Motivation

One of the defining characteristics of Clojure is that it revolves
around a minimal set of basic data structures.

I think it can be argued that the collection primitives in Clojure can
be approximately ordered by their degree of expressiveness:

- seq - first/rest
- < set - membership
- < vector - indexable members
- < map - a collection of associations

The conceit of IGraph is that there is room for a new collection
primitive with one higher level of expressiveness:

- < graph - a collection of named relationships between named entities

This is informed to a large degree by the
[RDF](https://www.wikidata.org/wiki/Q54872) model, and aims to align
with [linked data](https://www.wikidata.org/wiki/Q515701) encoded in
RDF, while keeping direct dependencies to a minimum.

<a name="h2-igraph-protocol"></a>
## The IGraph protocol

This protocol defines the basic operations over a graph conceived of
as a set of triples S-P-O, where subject `S` and object `O` typically
name entities, and property `P` is a named relation that applies
between those entities.

This is directly inspired by the RDF model, but the requirement that
these identifiers adhere strictly to RDF specifications for URIs, and
that literal values be restricted to a small set of scalars is relaxed
quite a bit.

<a name="h3-methods-summary"></a>
### Methods summary

The `IGraph` protocol specifies the following methods:

#### Member access
- `(normal-form g)` -> `{s {p #{o...}...}...}`
- `(subjects g)` -> `(s ...)`, a lazy sequence
- `(get-p-o g s)` -> `{p #{o...} ...}`
- `(get-o g s p)` -> `#{o ...}`
- `(ask g s p o)` -> truthy
- `(query g q)` -> implementation-dependent query results

#### Content manipulation
- `(mutability g)` -> One of `#{::read-only ::immutable ::mutable :accumulate-only}`

#### `invoke` to support `IFn`
- `(g)` = `(normal-form g)`
- `(g s)` -> {p #{o...} ...}
- `(g s p)` -> #{o ...}
- `(g s p o)` -> truthy

<a name="Member_access"></a>
### Member access

<a name="Normal_form"></a>
#### Normal form

Any implemetation of this protocol, regardless of its _native
representation_ must be expressable in IGraph's `Normal Form`.

As an example, let's start with a graph called 'eg' with four triples:

```
> (igraph/normal-form eg)
{:john 
  {:isa #{:person}, 
   :likes #{:beef}},
 :mary 
 {:isa #{:person}, 
  :likes #{:chicken}}}
>
```

These are facts about two subjects, :john and :mary with two facts
each.

John is a person who likes meat.

Mary is also a person, and likes chicken.


The normal form has three tiers. The "s-level" is a map from each
subject to a "p-level" `description` of that subject.  The normal form
for descriptions is a map from a property identifier to an "o-level"
set of objects for said subject and property.

What I'm aiming for here is a form that's
- extremely regular and simple
- lends itself to expressing and thinking about basic set operations
on graphs.

##### A note on the keyword identifiers used in these examples

To keep things simple and readable, none of the keywords used in these
examples are [namespaced](https://blog.jeaye.com/2017/10/31/clojure-keywords/#namespaced-keywords). 

In practice you will probably want to used namespaced keywords, and
some implementations of IGraph, e.g. those that interact directly with
RDF-based representations, will expect them.


<a name="h4-tractability"></a>
#### Tractability

It is expected that while many implementations of IGraph will be
in-memory data structures of modest size, others might be huge
knowledge bases provided on a server somewhere
([Wikidata](https://www.wikidata.org/wiki/Q2013), for example). In the
latter case it is always acceptable throw an `::igraph/Intractable`
for any method that warrants it:

```
(throw (ex-info "Normal form for Wikidata is intractable" 
  {:type ::igraph/Intractable}))

```


<a name="subjects_method"></a>
#### `subjects`

The `subjects` method must return a lazy sequence of complete set of
subjects in the graph (modulo tractability):

```
> (igraph/subjects eg)
`(:john :mary)
> (type (igraph/subjects eg))
clojure.lang.LazySeq
>
```

<a name="get-p-o_method"></a>
#### `get-p-o`

We must be able to get the p-level description of any subject with
`get-p-o`:

```
> (igraph/get-p-o eg :john)
{:isa #{:person}, :likes #{:beef}}
>
```

<a name="get-o_method"></a>
#### `get-o`

We must be able to get the o-level set of objects for any subject and
predicate with `get-o`:

```
> (igraph/get-o eg :john :isa)
#{:person}
>
``` 

<a name="ask_method"></a>
#### `ask`

We must be able to test for whether any particular triple is in the
graph with `ask` (any truthy response will do).

``` 
> (igraph/ask eg :john :likes :beef) 
:beef 
> (igraph/ask eg :john :likes :chicken) 
nil
>
```

<a name="query_method"></a>
#### `query`

We must be able to query the graph using a format appropriate to the
native representation. This example uses the format expected by
`ont-app.igraph.graph/Graph`, described [below](#h4-querying):

```
> (igraph/query eg [[:?person :isa :person]])
#{{:?person :mary} {:?person :john}}
>
```

In this case, the result is a set of `binding maps`, mapping
:?variables to values, similar to the result set of a SPARQL query.

For comparison, here is a sketch of an equivalent SPARQL query, which
would be appropriate if our IGraph protocol was targeted to a SPARQL
endpoint which we might call `sparql-eg`:

```
> (query sparql-eg 
  "PREFIX : <http://path/to/my/ns#> 
   SELECT * WHERE 
   {
     ?person :isa :person
   }")
[{:person :mary} {:person :john}]
> 
```

<a name="invoke_method"></a>
#### `invoke` for arities 0-3

An instance of IGraph must provide `invoke` implementations as
follows:

Without arguments, it must return Normal Form (or throw an ::igraph/Intractable):

```
> (eg)
{:john {:isa #{:person}, :likes #{:beef}},
 :mary {:isa #{:person}, :likes #{:chicken}}}
>

```

With a single "S" argument, it must treat the argument as the subject
of get-p-o:

```
> (eg :john)
{:isa #{:person}, :likes #{:beef}},
>
```

With two arguments "S" and "P", a set of objects must be returned:

```
> (eg :mary :likes)
#{:chicken}
>
```

This will often be the value of `get-o`, but it may also accept as the
"p" argument a _traversal function_, described
[below](#traversal-fn-as-p).

With three arguments "S" "P" and "O", the response must be truthy:

```
> (eg :mary :likes :chicken)
:chicken
>
>
> (eg :mary :likes :beef)
nil
>
```

This will often be equivalent to `ask`, but again, the "P" argument
can be a traversal function, described [below](#traversal-fn-as-p).

<a name="Content_Manipulation"></a>
### Content Manipulation

There a various factors to take into account when adding or removing
content from a graph.

Some graphs, (such as a public SPARQL endpoint to which one does not
have UPDATE permissions) may not be subject to modification. Other
native representations (such as a SPARQL endpoint with UPDATE
permissions) might best be treated as mutable graphs.

Naturally, other things being equal, the preferred solution is to use
immutable graphs when it is possible to do so. The examples in this
README will all be applied to immutable graphs.

<a name="mutability_method"></a>
#### `mutability`

The `mutability` method returns one of the following values
- `::igraph/read-only` - there is no means for altering the contents of
  the graph
- `::igraph/immutable` - the graph implements
  [IGraphImmutable](#IGraphImmutable)
- `::igraph/mutable` - the graph implements
  [IGraphMutable](#IGraphMutable)
- `::igraph/accumulate-only - the graph implements [IGraphAccumulateOnly](#IGraphAccumulateOnly), the approach used in Datomic

<a name="add-to-graph"></a>
#### The `add-to-graph` multimethod

IGraph defines a multimethod `add-to-graph`, dispatched on the type of
graph, and a function `triples-format`. This multimethod can inform
both mutable and immutable graphs.

Naturally Normal Form is one possible format:

```
> (igraph/triples-format {:john {:likes# #{:beef}}})
:normal-form
>
```

Another possible value is `:vector`, with a subject and an odd number
of P-O specifications:

```
> (igraph/triples-format [:john :likes :beef])
:vector
> (igraph/triples-format [:john :isa :person :likes :beef])
:vector
>
``` 

Finally, we have `:vector-of-vectors`:

```
> (igraph/triples-format  [[:john :isa :person] [:mary :isa :person]])
:vector-of-vectors
>
```

Any implementation of IGraph should support adding to the graph in all
of these formats.

<a name="remove-from-graph"></a>
#### The `remove-from-graph` multimethod

IGraph also defines multimethod `remove-from-graph`, dispatched on the
graph types and a function `triples-removal-format`. This multimethod
can inform both mutable and immutable graphs.

The `triples-removal-format` function returns the same keywords as
`triples-format`, but adds one more: `:underspecified-triple`, a
vector with fewer than 3 elements:

```
> (igraph/triples-removal-format [:john])
:underspecified-triple
> (igraph/triples-removal-format [:john :likes])
:underspecified-triple
>
```

`triples-removal-format` assigns the :vector-of-vectors flag a vector
of either :vector or :underspecified-vector. All implementations of
IGraph should support each of these flags.

This allows us to subtract any format that could also be added, plus
all `[s * *]` or all `[s p *]`.



<a name="IGraphImmutable"></a>
## The IGraphImmutable protocol

An add or subtract operation to an immutable graph returns a cheap
copy of the original graph modified per the argument provided.

<a name="add_method"></a>
### `add`

Calling `(add g to-add)` must return an immutable graph such that the
graph now contains `to-add`. Any triples in `to-add` which are already
in the graph are allowed, but implementations should not duplicate
identical triples in the graph.

See the notes above about the [add-to-graph](#add-to-graph)
multimethod.

Typically adding to a graph in code is most easily expressed using a
vector or a vector of vectors:

```
> (igraph/normal-form 
    (igraph/add 
      eg 
      [[:chicken :subclass-of :meat]
       [:beef :subclass-of :meat]
       ]))
{:john {:isa #{:person}, :likes #{:beef}},
 :mary {:isa #{:person}, :likes #{:chicken}},
 :chicken {:subclass-of #{:meat}},
 :beef {:subclass-of #{:meat}}}
>
```

We can add use Normal Form of one graph to add it to another.

```
> (meats)
{:chicken {:subClass-of #{meat}}
 :beef {:subClass-of #{meat}}}
>
> (igraph/normal-form (add eg (meats)))
{:john {:isa #{:person}, :likes #{:beef}},
 :mary {:isa #{:person}, :likes #{:chicken}},
 :chicken {:subclass-of #{:beef}},
 :beef {:subclass-of #{:beef}}}
> 
```

<a name="subtract_method"></a>
#### `subtract`

The multimethod `remove-from-graph` supports the `subtract` operation,
dispatched on the type of the graph and `triples-removal-format`,
described [above](#remove-from-graph):

```
> (igraph/normal-form (igraph/subtract eg [:john]))
{:mary {:isa #{:person}, :likes #{:chicken}}}
>
> (igraph/normal-form (igraph/subtract eg [:john :likes]))
{:john {:isa #{:person}}, 
 :mary {:isa #{:person}, :likes #{:chicken}}}
>
```

<a name="IGraphMutable"></a>
## The IGraphMutable protocol

Some graphs' native representations are implemented as mutable
repositories. To support this, the IGraphMutable protocol provides
methods `add!` and `subtract!`.

The [add-to-graph](#add-to-graph) and
[remove-from-graph](#remove-from-graph) multimethods should still
inform the logic here, and the behavior should be essentially the
same, with the exception that the graph returned is the same mutated
object as was passed in to either of these methods.

<a name="add!_method"></a>
### `add!`

`(add! g to-add)` -> g, where g is both the argument and return value.

An error should be thrown if `(mutablility g)` != :igraph/mutable.

<a name="subtract!_method"></a>
### `subtract!`

`(subtract! g to-subtract)` -> g, where g is both the argument and
return value.

An error should be thrown if `(mutablility g)` != :igraph/mutable.

<a name="IGraphAccumulateOnly"></a>
## The IGraphAccumulateOnly protocol

A graph whose native representation is based on
[Datomic](https://www.datomic.com/) exects what Datomic calls an
"Accumulate-only" approach to adding and removing from a graph. To
support this, the IGraphAccumulateOnly protocol provides methods
`claim` (corresponding to the datomic 'add' operation), and
`retract`. In this scheme the state of the graph can be rolled back to
any point in its history. See the Datomic documentation for details.

The [add-to-graph](#add-to-graph) and
[remove-from-graph](#remove-from-graph) multimethods should still
inform the logic here, and the behavior should be essentially the
same, with the exception that the graph returned now points to the
most recent state of the graph after making the modification. Any
given instantiation of the graph will remain immutable.

<a name="claim_method"></a>
### `claim`

`(claim g to-add)` -> g', where g is an append-only graph, and
g' now points to the most recent state of g's
[transactor](https://docs.datomic.com/on-prem/transactor.html).

An error should be thrown if `(mutablility g)` != :igraph/accumulate-only.

<a name="retract_method"></a>
### `retract`

`(retract g to-retract)` -> g', where g is an append-only graph, and
g' now points to the most recent state of g's
[transactor](https://docs.datomic.com/on-prem/transactor.html).

An error should be thrown if `(mutablility g)` != :igraph/accumulate-only.


<a name="h2-igraphset-protocol"></a>
## The IGraphSet protocol

It will make sense for many implementations of IGraph also to
implement the basic set operations, defined in IGraphSet. Set
operations may not be suitable between very large graphs.

For purposes of demonstration, let's assume a second graph `other-eg`:

```
> (igraph/normal-form other-eg)
{:mary {:isa #{:person}, :likes #{:pork}},
 :waldo {:isa #{:person}, :likes #{:beer}}}
>
```

I think examples of each operation should serve to describe them.

<a name="h3-igraphset-methods-summary"></a>
### Methods summary
- `(union g1 g2)` -> A new graph with all triples from both graphs
- `(difference g1 g2)` -> A new graph with triples in g1 not also in
  g2
- `(intersection g1 g2)` -> A new graph with only triples shared in
  both graphs

<a name="union_method"></a>
### `union`
```
> (igraph/normal-form (igraph/union eg other-eg))
{:john {:isa #{:person}, :likes #{:beef}},
 :mary {:isa #{:person}, :likes #{:pork :chicken}},
 :waldo {:isa #{:person}, :likes #{:beer}}}
>

``` 

<a name="intersection_method"></a>
### `intersection`

```
> (igraph/normal-form (igraph/intersection eg other-eg)) 
{:mary {:isa #{:person}}
>
```

<a name="difference_method"></a>
### `difference`
```
> (igraph/normal-form (igraph/difference eg other-eg))
{:john {:isa #{:person}, :likes #{:beef}}, 
 :mary {:likes #{:chicken}}}
>
> (igraph/normal-form (igraph/difference other-eg eg))
{:mary {:likes #{:pork}}, :waldo {:isa #{:person}, :likes #{:beer}}}
>
```

<a name="Traversal"></a>
## Traversal

Clojure and other functional programming languages have a
[reduce](https://clojuredocs.org/clojure.core/reduce) idiom, which
allows the user to create aggregations over a sequence by providing a
"reducer function" expressing the relationship between each member of
that sequence and the resulting aggregation.

IGraph defines a `traverse` function to allow the user to create
aggregations over the contents of a graph by providing a `traversal
function`, which is analogous to a reducer function, but is
nessesarily a bit more involved.


- `(traverse g traversal context acc queue)` -> `acc'`

    ... traversing `g` per the `traversal` function, starting with the
    first element of `queue`, possibly informed by `context`.

This function will repeatedly call the `traversal` function until
`queue` is empty, returning the final value for `acc`. Each call to
the traversal function returns modified versions of `context`, `acc`
and `queue`.

To illustrate traversal, let's expand on our `eg` graph by adding some
type structure:

Assume we have a graph called 'eg-with-types':

```
> (def eg-with-types 
    (add eg
      [[:person :subClassOf :thing]
       [:beef :subClassOf :meat]
       [:chicken :subClassOf :meat]
       [:meat :subClassOf :food]
       [:beer :subClassOf :beverage]
       [:beverage :subClassOf :consumable]
       [:food :subClassOf :consumable]
       [:consumable :subClassOf :thing]]))
eg-with-types
> (eg-with-types)
{:consumable {:subClassOf #{:thing}},
 :beef {:subClassOf #{:meat}},
 :person {:subClassOf #{:thing}},
 :beer {:subClassOf #{:beverage}},
 :meat {:subClassOf #{:food}},
 :food {:subClassOf #{:consumable}},
 :beverage {:subClassOf #{:consumable}},
 :pork {:subClassOf #{:meat}},
 :john {:isa #{:person}, :likes #{:beef}},
 :mary {:isa #{:person}, :likes #{:chicken}},
 :chicken {:subClassOf #{:meat}}}
```

Our `eg-with-types` now provides a bit more context for what's going
on with our heroes John and Mary.

<a name="traverse_function"></a>
### The `traverse` function

Here's an example of how the `traverse` function works, starting with
a traversal function we'll call `subClassOf*`, which follows and
accumulates all :subClassOf links, starting with an initial queue of
say, `[:meat :beer]`:

```
> (igraph/traverse eg-with-types subClassOf* {} #{} [:meat :beer])
#{:consumable :beer :meat :food :beverage :thing}
>
```

The arguments for `traverse` are
- `g` - an invariant graph
- `traversal-fn` - A function `[g c acc q]` -> `[c' acc' q']`,
  defining the logic of each step in the traversal
- `context` - (optional) a map holding the traversal history plus
  whatever `traversal-fn` may want to track. Default is {}
- `acc` - accumulates the resulting value of the traversal
- `queue` - the starting queue

<a name="Traversal_functions"></a>
#### Traversal functions

The traversal function takes 4 arguments and returns a vector of
length 3.

```
> (subClassOf* eg-with-types {} #{} [:meat])
[{} #{:meat} (:food)]
>
> (subClassOf* eg-with-types {} #{:meat} '(:food))
[{} #{:meat :food} (:consumable)]
>
```

The first argument is the invariant graph itself.

The second argument (and first element returned) is the context, which
subClassOf* leaves unchanged.  Context is used by `traverse` to avoid
cycles, and will be explained in detail [below](#h4-context), but let us
state here that more sophisticated traversal functions may use the
context as a kind of blackboard to guide the traversal.

The third argument (and second element returned) is the value to be
accumulated, identical to its counterpart in the _reduce_ idiom.

The fourth argument (and third element returned) is the traversal
queue. It must be sequential, and may be ordered in any way that makes
sense. An empty queue signals and end of the traversal, at which point
`traverse` will return the value of the accumulator.

Here's a possible definition of subClassOf*:

```
(defn subClassOf* [g c acc q]
  "Traversal function to accumulate super-classes."
  (let [elt (first q)]
    [c                      ;; context is unchanged
     (conj acc elt)         ;; updating the accumulator
     (reduce conj 
       (rest q) 
       (g elt :subClassOf)) ;; adding super-classes to the queue
       ]))
```

<a name="h4-context"></a>
#### Context

The `context` argument to `traverse` and its traversal function is a
map containing key-values which may inform the course of the
traversal, but are not part of the accumulated value. This will
include:
- `:history` set by `traverse`, and updated to hold all elements
  encountered in the course of the traversal. In order to avoid
  cycles, any element in the history will be skipped should it ever
  re-appear at the head of the queue.
  
The `traverse` function also supports these optional keys in the
context:

- `:skip?` (optional), a function (fn[x] ...) -> truthy, applicable to
  the head of the queue, which will override `:history`.
  
- `:seek` (optional), a function `(fn [context acc]...)` -> `acc'`. If
  specified, this function will be called at the beginning of each
  traversal, and if truthy and non-empty, the traversal will end
  immediately with that value.

In addition, the traversal function may use the context as a
blackboard to communicate between iterations of the traversal. As an
example, you may want to prune and re-order your queue based on a set
of heuristics, details of which are stored in the context.

<a name="queue"></a>
#### The queue

The `queue` argument must be sequential, but is otherwise
unrestricted. An empty queue signals the end of the traversal, at
which point `traverse` will return the accumulated value.

Note that conj-ing to a vector in the traversal function suggests a
breadth-first traversal, while conj-ing to a seq suggests a
depth-first tranversal.

More sophisticated traversal functions may use the context to inform
logic to prune and re-order the queue to optimize the traversal.

<a name="Traversal_utilities"></a>
### Traversal utilities

IGraph provides utilities to express several common types of traversal
functions.

<a name="h4-transitive-closure"></a>
#### `transitive-closure`

- `(trasitive-closure p)` -> `(fn [g context acc to-visit] ...) ->
  [context' acc' queue']`,
  
  This returns a traversal function which will accumulate all `o`
  s.t. any `s` in the queue is associated with `o` through zero or
  more `p` links.

So in the example above, the `subClassOf*` function could be defined
thus:

```
(def subClassOf* (igraph/transitive-closure :subClassOf))
```

<a name="h4-traverse-link"></a>
### `traverse-link`

- `(traverse-link p)` -> (fn [g context acc queue] ...) -> [context
  acc' []],

The function returned here will accumulate all `o` s.t. for all `s` in
`queue`, (g s p o) is truthy:

```
> (igraph/traverse 
    eg-with-types 
    (igraph/traverse-link :isa) 
    #{} 
    [:john :mary])
#{:person}
>
```

<a name="h4-maybe-traverse-link"></a>
#### `maybe-traverse-link`

- `(maybe-traverse-link p)` -> (fn [g context acc queue] ...) ->
  [context acc' []]

Matches 0 or 1 occurrences of `p`:

```
> (igraph/traverse eg-with-types 
    (igraph/maybe-traverse-link :isa) 
    #{} 
    [:john :mary])
#{:person :john :mary}
>
```

<a name="h4-traverse-or"></a>
#### `traverse-or`

- `(traverse-or & ps)` -> (fn [g context acc queue] ...) -> [context
  acc' []],

Where `ps` is one or more traversal functions, merging all of their outputs.

Keyword arguments are interpreted as an implicit `traverse-link`.

```
> (def subsumed-by (igraph/traverse-or :isa :subClassOf))
subsumed-by
> (igraph/traverse eg-with-types subsumed-by #{} [:john])
#{:person}
>
> (igraph/traverse eg-with-types subsumed-by #{} [:meat])
#{:food}
>
```

<a name="Traversal_composition"></a>
### Traversal composition with `t-comp`
Composition functions are composable with a 'short form' and a 'long
form'.

<a name="h4-t-comp-short"></a>
#### short form

Short-form composition can be used when the traversal function meets
the following criteria:
- None of the component functions manipulate the traversal context
- Each component function accumulates a sequential value suitable to
  serve as the initial queue of the component function that follows
  it.

Such functions can be called as a simple vector:

```
> (def instance-of 
    (igraph/t-comp [:isa (igraph/transitive-closure :subClassOf)]))
>
> (igraph/traverse eg-with-types instance-of #{} [:john])
#{:person :thing}
>
```

<a name="h4-t-comp-long"></a>
#### long form

In cases where one wants to compose a traversal function that cannot
meet the criteria above, then instead of passing to `traversal-comp` a
vector of traversal functions, you pass in a map with the following
keys:

```
{ :path  [:<traversal-stage-1> :<traversal-stage-2> ...]
   :<traversal-stage-1> {:fn <traversal-fn>
                      :doc <docstring> (optional)
                      :into <initial accumulator> (default [])
                      :local-context-fn <context> (default nil)
                      :update-global-context (default nil)
                      }
   :<traversal-stage-2> ...
   ...
 }
 ```
 
A call to `(t-comp [:a :b :c])` is equivalent to calling `(t-comp
{:path [:a :b :c]})`.

These parameters should allow you as much control as you need over the
flow of contexts between each stage of traversal, and over the flow of
outputs from any one stage into the input queue of its next stage.

However, most of the time, the short form is sufficient, and at this point,
the long form has not been tested heavily.

##### the :path parameter

This is a vector of traversal function specifications. Each traversal
function specification must be either:
- A traversal function
- A keyword with an entry in the long-form map
- A keyword eligible as an implicit [traverse-link](#h4-traverse-link)

If the traversal function specification is itself a function, it will
be applied directly.

If the traversal function specification is a keyword, and the t-comp
map has a matching entry for that keyword, it will look for and
interpret a map with the parameters described in the next section.

If the spec is a keyword without an entry in the long-form map, it is
assumed to be a candidate for an implicit traverse-link, i.e. a graph
element in 'p' position in `g`.

##### traversal specification parameters

- :fn - associated with a traversal function
- :doc (optional) - a docstring
- :into (optional) - a container to which the output should be coerced
  (default [])
- :local-context-fn (optional) - a function [global-context] -> `local
  context` producing the context for this stage of the traversal.
- :update-local-context (optional) - a function [global-context
  local-context] -> `global-context'`, capturing whatever aspects of
  the current stage of traversal may be of interest to subsequent
  stages.

<a name="traversal-fn-as-p"></a>
### Using traversal functions as a `p` argument to `invoke`

Recall that implementations of IGraph should provide `invoke`
functions with 0-3 arguments.

Two of these functions involve specification of a `p` parameter:

```
(g s p) -> {<o>...}

(g s p o) -> truthy.
```

This is informed by a multimethod dispatched on whether `p` is a
function:

- `(match-or-traverse g s p)` -> #{<o>...}  
- `(match-or-traverse g s p o)` -> truthy

A typical declaration for an IGraph implementation will contain
these two method declarations:

```
  #?(:clj clojure.lang.IFn
     :cljs cljs.core/IFn)
  ...
  (invoke [g s p] (igraph/match-or-traverse g s p))
  (invoke [g s p o] (igraph/match-or-traverse g s p o))
  ...
```

If the `p` argument is a function, then `p` will be expected to match
the signature of a traversal function, and the output of the method
will be the value of its traversal, starting with queue [`s`].

If `p` is not a function it will be matched directly against elements
of the graph.

So given the traversal functions in the examples above:

```
> (eg-with-types :beef subClassOf*)
#{:consumable :beef :meat :food :thing}
>
> (eg-with-types :beef subClassOf* :food)
:food
>
> (eg-with-types :john (igraph/t-comp [:likes subClassOf*]))
#{:consumable :beef :meat :food :thing}
>
```



<a name="cardinality-1_utilities"></a>
## cardinality-1 utilites

Requiring normal form to provide a set as its 3rd-tier representation
has the advantage of ensuring that the normal form is as simple and
regular as possible, and makes it easy to think about set operations
over graphs. However, it can be a bit unwieldy when dealing with the
many cases where the descriptive map's keys reliably map to a single
scalar value.

The following utilities are provided to help with this:

- `(unique [x]) -> x` - translates a singleton sequence to its only
  value
- `(flatten-description (g s))` Automatically translates the p-o
  description into a simple k-v mappings wherever only a single `v`
  exists.
- `(normalize-flat-description m)` is the inverse of
  `flatten-description`.
- `(assert-unique g s p o) - replaces one singleton object with
  another.

<a name="h3-unique"></a>
### `unique`

The `unique` function takes a sequence and an optional `on-ambiguity`
argument. Default on-ambiguity throws ex-info of type
`::igraph/Non-unique`.

```
> (eg-with-types :john :isa)
{:person}
>
> (igraph/unique (eg-with-types :john :isa))
:person
>
> (igraph/unique (eg-with-types :beef subClassOf*))
Execution error (ExceptionInfo) at ont-app.igraph.core/unique$fn (core.cljc:640).
Unique called on non-unique collection
>
> (igraph/unique (eg-with-types :beef subClassOf*)
                 first)
:consumable
```

Sometimes defining `the` as an alias for `unique` reads better, and is
easier to type:

```
> (def the igraph/unique)
> (the (eg-with-types :john :isa))
:person
>
```

<a name="h3-flatten-description"></a>
### `flatten-description`

```
(igraph/flatten-description (eg-with-types :john))
{:isa :person, :likes :beef}
>
> (let [g (igraph/add 
             eg 
            [:john :likes :beer :has-vector [1 2 3]])
        ]
    (igraph/flatten-description (g :john)))
{:isa :person, :likes #{:beef :beer}, :has-vector [1 2 3]}
>
```

<a name="h3-normalize-flat-description"></a>
### `normalize-flat-description`

This is the inverse of `flatten-description`:

```
> (igraph/normalize-flat-description 
    {:isa :person, :likes #{:beef :beer}, :has-vector [1 2 3]})
{:isa #{:person}, :likes #{:beef :beer}, :has-vector #{[1 2 3]}}
>
> (let [g (igraph/add 
             eg 
             {:john (igraph/normalize-flat-description {:likes :beer})})
        ]
    (g :john))
{:isa #{:person}, :likes #{:beef :beer}}
>
```

<a name="h3-assert-unique"></a>
### `assert-unique`

We can replace one singleton value with another using `(assert-unique
g s p o) -> g'`:

```
> (let [g (igraph/assert-unique eg :john :isa :man)]
    (g :john))
{:likes #{:beef}, :isa #{:man}}
>
```

<a name="i-o"></a>
## I/O

In general writing the normal form of a graph to a stream and applying
the reader to it on the other end should be fairly
straightforward. Any predicates bearing reader-choking objects will of
course need to be filtered out.

At this point, only the :clj platform is directly supported with a
pair of functions to read/write to the file system.

<a name="h3-write-to-file"></a>
### `write-to-file`
`(write-to-file [path g] ...) -> path`

Will write an edn file with the normal form contents of `g`.

<a name="h3-read-from-file"></a>
### `read-from-file`

`(read-from-file [g path] ...) -> g'`

Will read the normal form contents of `path` into `g`.

<a name="Other_utilities"></a>
## Other utilities

<a name="h3-reduce-spo"></a>
### `reduce-spo`
- `(reduce-spo f acc g)` -> `acc'`, such that `f` is called on each triple in
`g`.  Where `f` := `(fn [acc s p o]...) -> acc'`. Cf. [reduce-kv](https://clojuredocs.org/clojure.core/reduce-kv)

```
> (defn tally-triples [tally s p o]
    (inc tally))
> (igraph/reduce-spo tally-triples 0 eg)
4
```
<a name="h2-implementations"></a>
## Implementations

The `ont-app.igraph.graph` module makes one implementation of IGraph
available without any additional dependencies, and so far there are
two other libraries in the ont-app project which implement this
protocol.

Other implementations are planned, and I'd be interested to learn of
any implementations published by other parties.

<a name="Graph"></a>
### `ont-app.igraph.graph/Graph`

The IGraph library comes with `ont-app.igraph.graph`, whose Graph
deftype is a very lightweight implementation of IGraph.

Its native representation is just Normal Form. Any hashable object can
technically be provided for any `s`, `p`, or `o`, but best practice is
to keep non-identifiers a the `o` level if you want to play easily
with other IGraph implementations.

```
(require '[ont-app.igraph.graph :as g])
```

<a name="h4-graph-creation"></a>
#### Graph creation

Use `make-graph` to create a new graph, with an optional `:contents`
argument.

```
> (def eg (g/make-graph))
eg
> (eg)
{}
>
> (def eg
    (g/make-graph 
      :contents {:john {:isa #{:person}, :likes #{:beef}},
                 :mary {:isa #{:person}, :likes #{:chicken}}}
eg
> (eg)
{:john {:isa #{:person}, :likes #{:beef}},
 :mary {:isa #{:person}, :likes #{:chicken}}}
>
```

The `:contents` argument must be in Normal Form.


<a name="h4-querying"></a>
#### Querying

Querying is done with a very simple vector-of-triples graph pattern
using keywords starting with ":?" to serve as variables. It returns an
unordered set of binding maps. This is very minimalistic. Any
selecting, ordering, grouping or aggregation needs to be done
downstream from the call.

```
> (igraph/query eg [[:?liker :likes :?liked]])
#{{:?liker :john, :?liked :beef} 
  {:?liker :mary, :?liked :chicken}}
>
```

Traversal functions can be specified in `p` position:

```
> (igraph/query eg-with-types [[:?liker :likes ?liked]
                               [?liked subClassOf* :?liked-class]])
#{{:?liked :beef, :?liked-class :consumable, :?liker :john}
  {:?liked :beef, :?liked-class :beef, :?liker :john}
  {:?liked :chicken, :?liked-class :food, :?liker :mary}
  {:?liked :chicken, :?liked-class :chicken, :?liker :mary}
  {:?liked :chicken, :?liked-class :consumable, :?liker :mary}
  {:?liked :beef, :?liked-class :meat, :?liker :john}
  {:?liked :beef, :?liked-class :food, :?liker :john}
  {:?liked :beef, :?liked-class :thing, :?liker :john}
  {:?liked :chicken, :?liked-class :thing, :?liker :mary}
  {:?liked :chicken, :?liked-class :meat, :?liker :mary}}
>
```


<a name="h3-sparql-client"></a>
### sparql-client

<https://github.com/ont-app/sparql-client>

Implements IGraph for a [SPARQL
endpoint](https://www.wikidata.org/wiki/Q26261192). Initializtion
requires configuring query and update endpoints, and the query
language is [SPARQL](https://www.wikidata.org/wiki/Q54871).

Keyword identifiers are expected to be namespaced, and rely on the [ont-app/vocabulary](https://github.com/ont-app/vocabulary) library, which uses namespace metadata to intercede between Clojure namespaces and RDF namespaces.

<a name="h3-datascript-graph"></a>
### datascript-graph

<https://github.com/ont-app/datascript-graph>

This implements IGraph for a
[datascript](https://github.com/tonsky/datascript) native
representation, and may as such may need to be initialized with some
schema declarations. Query language is datalog. Immutable, with set
operations.

<a name="h3-datomic-client"></a>
### datomic-client

https://github.com/ont-app/datomic-client

This implements IGraph for the [Datomic Client API](https://docs.datomic.com/cloud/client/client-api.html). The query language is datalog. Mutability model is Accumulate Only. There are no set operations.

<a name="h2-future-work"></a>
## Future work
- loom, ubergraph, and other graph-oriented libraries will be
  ported to.
- There will be an annotated-normal-form, providing annotations for
  reified triples (for weights and such).
- `igraph.graph` will have query planning and indexing.
- Some kind of a scheme to bring all the various query formats under a
  single tent.

<a name="h2-license"></a>
## License

Copyright © 2019 Eric D. Scott

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

<table>
<tr>
<td width=75>
<img src="http://ericdscott.com/NaturalLexiconLogo.png" alt="Natural Lexicon logo" :width=50 height=50/> </td>
<td>
<p>Natural Lexicon logo - Copyright © 2020 Eric D. Scott. Artwork by Athena M. Scott.</p>
<p>Released under <a href="https://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International license</a>. Under the terms of this license, if you display this logo or derivates thereof, you must include an attribution to the original source, with a link to https://github.com/ont-app, or  http://ericdscott.com. </p> 
</td>
</tr>
<table>
