# -*- coding: utf-8 -*-
require 'graph/sugiyama/version'
require 'graph/implementation'

require 'set'

module Graph::Sugiyama

  private

  public

  class << self

    # Returns a modified graph and the set of edges that were reversed.
    def remove_cycles graph
      graph = Graph::Implementation.from_rgl graph unless
        graph.is_a? Graph::Implementation

      # give us a duplicate
      g = graph.dup

      # what follows is algorithm 13.3 (greedy cycle removal) from
      # healy/nikolov graph drawing handbook chapter 13 on hierarchical
      # drawing algorithms.

      # left and right node sequences
      seql = []
      seqr = []

      until g.empty? do
        while v = g.nodes.detect { |n| g.sink? n }
          g.remove v
          seqr.unshift v # prepend to sink the right side
        end

        while u = g.nodes.detect { |n| g.source? n }
          g.remove u
          seql.push u # append the source to the left side
        end

        unless g.empty?
          # pick the first node with the highest net degree
          w = g.nodes.sort do |a, b|
            x = (g.outdegree(b) - g.indegree(b)) <=>
              (g.outdegree(a) - g.indegree(a))
            # fall back to lexical sort to guarantee consistent results
            x == 0 ? a <=> b : x
          end.first

          g.remove w
          seql.push w
        end
      end

      # now any edges going from higher up the sequence to lower
      # constitute a feedback arc set. we make this into a hash so we
      # can look it up.
      seq = {}
      (seql + seqr).each_with_index { |x, i| seq[x] = i }

      # give us a new duplicate
      g = graph.dup

      # invert the edges on the duplicate
      graph.edges { |s, t| g.invert s, t if seq[s] > seq[t] }

      # return the duplicate
      g
    end

    private

    # GKNV93 p. 9: An initial feasible ranking is computed. For
    # brevity, init_rank is not given here. Our version keeps nodes in
    # a queue. Nodes are placed in the queue when they have no
    # unscanned in-edges. As nodes are taken off the queue, they are
    # assigned the least rank that satisfies their in-edges, and their
    # out-edges are marked as scanned. In the simplest case, where δ =
    # 1 for all edges, this corresponds to viewing the graph as a
    # poset and assigning the minimal elements to rank 0. These nodes
    # are removed from the poset and the new set of minimal elements
    # are assigned rank 1, etc.

    # my interpretation: 
    def initial_rank graph # , delta = 1 # delta (min length) is tbd
      scanned = {} # { source => [target] }
      rank    = {} # { node => rank }
      # give us the nodes sorted by outdegree - indegree
      queue   = graph.nodes_by_degree

      # XXX THIS MAY LOOP FOREVER IF THE GRAPH HAS CYCLES
      until queue.empty?
        node = queue.shift # shift a node off the queue
        # test if inbound edges have been scanned
        nin  = graph.neighbours_in node
        if nin.empty? or
            nin.reject { |n| scanned.fetch(n, []).include? node }.empty?
          # highest indegree rank plus one; defaults to zero
          rank[node] = (rank.values_at(*nin).compact.max || -1) + 1

          # record the outbound edges as having been scanned
          s = scanned[node] ||= Set.new
          s |= graph.neighbours_out(node).to_set
        else
          # otherwise push the node back onto the end
          queue.push node
        end
      end

      # might as well return the hash?
      rank
    end

    # The function tight_tree finds a maximal tree of tight edges
    # containing some fixed node and returns the number of nodes in
    # the tree. Note that such a maximal tree is just a spanning tree
    # for the subgraph induced by all nodes reachable from the fixed
    # node in the underlying undirected graph using only tight
    # edges. In particular, all such trees have the same number of
    # nodes.
    def tight? graph, rank
      
    end

    def slack graph, rank, source, target

    end

    # GKNV93 p. 10: The init_cutvalues function computes the cut
    # values of the tree edges. For each tree edge, this is computed
    # by marking the nodes as belonging to the head or tail component,
    # and then performing the sum of the signed weights of all edges
    # whose head and tail are in different components, the sign being
    # negative for those edges going from the head to the tail
    # component.
    def initial_cut_values
    end

    def feasible_tree graph
      rank = initial_rank graph
      # { source => { target => cutvalue } }

      [tree, rank]
    endg

    def leave_edge tree
    end

    def enter_edge tree
    end

    public

    # Returns a modified graph (with dummy nodes) and an array of layer
    # assignments.
    def assign_layers graph, concentrate: false
      graph = Graph::Implementation.from_rgl graph unless
        graph.is_a? Graph::Implementation

      # { source => { target => cutvalue } }
      tree, rank = feasible_tree graph

      while e = leave_edge(tree)
      end

      normalize tree # rebases ranks starting at zero
      balance tree # moves ambivalent nodes to less-populated ranks

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
end
