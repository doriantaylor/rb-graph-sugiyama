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
    edges ||= fwd.dup

    # this is something we destroy
    n = nodes.dup

    # what follows is algorithm 13.3 from healy/nikolov graph drawing
    # handbook chapter 13 on hierarchical drawing algorithms

    sl = Set.new
    sr = Set.new

    until n.empty? do
      # detect sinks
      while v = n.keys.detect { |x| n[x][:outdegree] == 0 }
        remove v, n, fwd, rev
        sr << v
      end
      # detect sources
      while u = n.keys.detect { |x| n[x][:indegree] == 0 }
        remove u, n, fwd, rev
        sl << u
      end

      unless n.empty?
        w = n.keys.sort do |a, b|
          x = (n[b][:outdegree] - n[b][:indegree]) <=>
            (n[a][:outdegree] - n[a][:indegree])
          # sort
          x == 0 ? a <=> b : x
        end.first

        remove w, n, fwd, rev

        sl << w
      end
    end

    [graph, edges]
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
