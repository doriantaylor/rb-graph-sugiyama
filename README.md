# Graph::Sugiyama: Tooling for hierarchical layouts of directed graphs

The Sugiyama framework is a general strategy for laying out directed
graphs on a plane with a quasi-hierarchical orientation. The process
decomposes roughly into four parts:

1. Make the graph acyclic (for the purpose of this manipulation),
2. Rank the nodes into layers (Y-axis, assuming top-to-bottom drawing),
3. Minimize edge crossings (along the X-axis, between individual layers),
4. Fine-tune node geometry (X-axis positions).

The Sugiyama framework was first advanced in 1981. Since then,
research and experimentation has of course continued. The first three
steps of the framework, in particular, are their own areas of study,
each yielding new algorithms with distinct characteristics. The fourth
and final step, concerned with geometry, can vary according to the
desired properties of the layout.

> A number of the computational problems related to graph manipulation
> are in NP, and so exhaustive solutions are not feasible. Every once
> in a while, somebody comes up with a new heuristic that is faster,
> or produces qualitatively different results.

The most popular implementation of the Sugiyama framework is the
program `dot` in the Graphviz package from AT&T. Why reinvent the wheel?

* `dot` is a program, not a library, and its primary interface is a
  text stream using its domain-specific language. Rather than
  serialize a graph, pipe it out to `dot`, and parse back the result,
  we want a library interface to directly manipulate an in-memory data
  structure.
* We want to be able to intervene on different parts of the Sugiyama
  process, potentially generating results that vary from `dot`.

The purpose of this module is to accumulate the ways the Sugiyama
framework can vary, beginning with a reinterpretation of `dot`.

## References

* STT81
* GKNV93
* HN2013

## Synopsis

```ruby
require 'graph-sugiyama'

# we presume you already have a graph of some kind, such that
# you can make these structures:

nodes = [] # ... note you can omit nodes if the thing is entirely connected
edges = {} # ... edge structure takes the form { source => [target] }

# anyway initialize, lol

graph = Graph::Sugiyama.new nodes: nodes, edges: edges

# i dunno, possibly we can manipulate this thing somehow

# and then finally we compute the layout

graph.layout
# => [
# => # a matrix-ish structure of real and dummy nodes,
# => # dunno some kind of information for drawing edges
# => ]
```

## Documentation

Generated and deposited
[in the usual place](http://www.rubydoc.info/gems/graph-sugiyama/).

## Installation

Come on, you know how to do this:

    $ gem install graph-sugiyama

Or, [download it off rubygems.org](https://rubygems.org/gems/graph-sugiyama).

## Contributing

Bug reports and pull requests are welcome at
[the GitHub repository](https://github.com/doriantaylor/rb-graph-sugiyama).

## Copyright & License

Copyright 2020, [Dorian Taylor](https://doriantaylor.com/)

This software is provided under
the [Apache License, 2.0](https://www.apache.org/licenses/LICENSE-2.0).
