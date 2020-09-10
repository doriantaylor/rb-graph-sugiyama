require "graph/sugiyama/version"

require 'set'

module Graph::Sugiyama

  private

  # Disassemble an RGL::DirectedAdjacencyGraph (into a structure that
  # is almost identical)
  def prepare_rgl graph
    # we cache reverse edges as well as forward
    nodes, fwd, rev = {}, {}, {}

    graph.each_vertex do |s|
      # we cache the degrees so we don't have to recompute them every time
      nodes[s] ||= { indegree: 0, outdegree: 0 }

      graph.each_adjacent(s) do |o|
        nodes[o] ||= { indegree: 0, outdegree: 0 }

        (fwd[s] ||= Set.new) << o
        (rev[o] ||= Set.new) << s

        nodes[s][:outdegree] += 1
        nodes[o][:indegree]  += 1
      end
    end

    { nodes: nodes, fwd: fwd, rev: rev }
  end

  # create a reverse structure
  def make_rev edges
    rev = {}

    # iterate over the target values to make keys and add to a new set
    edges.each { |src, tgt| tgt.each { |t| (rev[t] ||= Set.new) << src } }

    rev
  end

  # remove a node from our internal graph
  def remove node, nodes, fwd, rev
    [[rev, fwd, :outdegree], [fwd, rev, :indegree]].each do |a, b, k|
      a[node].each do |s|
        b[s].delete? node # remove the backreference
        nodes[s][k] -= 1  # decrement the degree
      end
    end

    # now delete the node from each hash
    [fwd, rev, nodes].each { |x| x.delete node }

    node
  end

  def remove_edge s, t, nodes, fwd, rev
    fwd[s].delete t
    rev[t].delete s
    nodes[s][:outdegree] -= 1
    nodes[t][:indegree]  -= 1

    [s, t]
  end

  public

  # Returns a modified graph and the set of edges that were reversed.
  def remove_cycles graph: nil, nodes: {}, edges: {}
    # this 
    nodes, fwd, rev = if graph and graph.respond_to? :each_vertex
                        prepare_rgl graph
                      else
                        { nodes: nodes, fwd: edges, rev: make_rev(edges) } 
                      end.values_at(:nodes, :fwd, :rev)

    # this is something we keep
    edges = fwd.dup

    # this is something we destroy
    n = nodes.dup

    # what follows is algorithm 13.3 (greedy cycle removal) from
    # healy/nikolov graph drawing handbook chapter 13 on hierarchical
    # drawing algorithms.

    # left and right node sequences
    seql = []
    seqr = []

    until n.empty? do
      # detect sinks
      while v = n.keys.detect { |x| n[x][:outdegree] == 0 }
        remove v, n, fwd, rev
        seqr.unshift v # prepend to sink the right side
      end
      # detect sources
      while u = n.keys.detect { |x| n[x][:indegree] == 0 }
        remove u, n, fwd, rev
        seql.push u # append the source to the left side
      end

      unless n.empty?
        w = n.keys.sort do |a, b|
          # outdegree minus indegree gives us 
          x = (n[b][:outdegree] - n[b][:indegree]) <=>
            (n[a][:outdegree] - n[a][:indegree])
          # sort by node lexical representation if tied so we always
          # get the same result
          x == 0 ? a <=> b : x
        end.first

        remove w, n, fwd, rev

        seql.push w
      end
    end

    # now any edges going from higher up the sequence to lower
    # constitute a feedback arc set. we make this into a hash so we
    # can look it up.
    seq = {}
    (seql + seqr).each_with_index { |x, i| seq[x] = i }

    fas = edges.map do |s, tgt|
      # keep the leftward-pointing edges (source has higher seq number)
      [s, tgt.dup.keep_if { |t| seq[s] > seq[t] }]
    end.reject { |x| x.last.empty? }.to_h

    { nodes: nodes, edges: edges, fas: fas }
  end

  # Returns a modified graph (with dummy nodes) and an array of layer
  # assignments.
  def assign_layers graph, concentrate: false

    # [graph, layers]
  end

  # Returns a modified graph (again with EC dummy nodes) and a new
  # array of layer assignments.
  def concentrate_edges graph, layers

    # [graph, layers]
  end

  # Returns the layer assignment with the correct ordering.
  def reduce_crossings graph, layers, tighten: false

    # layers
  end

  # Tightens edges by aligning nodes longitudinally. Returns a layer
  # assignment the modified ordering, with columns possibly padded with `nil`.
  def tighten_edges graph, layers
    
    # layers
  end
end
