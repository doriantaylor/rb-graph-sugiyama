require 'graph/sugiyama/version'

require 'set'

# This is a basic little unlabeled directed graph implementation for
# use with the Sugiyama framework
class Graph::Implementation

  private

  def initialize_copy orig
    # just dup these members individually; they can all be treated the same
    %w[@nodes @fwd @rev].each do |sym|
      instance_variable_set sym,
        orig.instance_variable_get(sym).map { |k, v| [k, v.dup] }.to_h
    end
  end

  public

  # Convert a RGL::DirectedAdjacencyGraph into this graph type.
  #
  # @param graph [RGL::DirectedAdjacencyGraph] an input graph
  # @return [Graph::Implementation] a new graph
  #
  def self.from_rgl graph
    raise ArgumentError,
      'graph must look like an RGL::DirectedAdjacencyGraph' unless
      %i[directed? each_vertex each_adjacent].all? { |m| graph.respond_to? m }
    nodes = []
    edges = {}

    graph.each_vertex do |s|
      # extract nodes indiscriminately
      nodes << s
      # now extract edges
      graph.each_adjacent(s) { |o| (edges[s] ||= []) << o }
    end

    # now initialize
    self.new nodes: nodes, edges: edges
  end

  # Initialize the object.
  #
  # @param nodes [Array] An array of (possibly disconnected) nodes.
  # @param edges [Hash] A hash relating nodes to an array-like set of edges.
  #
  def initialize nodes: [], edges: {}
    @nodes = {} # node metadata (just degrees for now)
    @fwd   = {} # forward edges
    @rev   = {} # reverse edges

    # now add any elements
    nodes.each { |n| add n } 
    edges.each do |s, tgt|
      (tgt.respond_to?(:to_a) ? tgt.to_a : [tgt]).each { |t| add_edge s, t }
    end
  end

  # Test whether the graph is empty.
  #
  # @return [true,false]
  #
  def empty?
    @nodes.empty?
  end

  # Return the nodes in the graph.
  #
  # @yield [node] Iterate over the nodes.
  # @yieldparam node [Object] The node passed to the block.
  # @return [Array] The nodes.
  #
  def nodes &block
    block ? @nodes.each { |n, _| block.call n } : @nodes.keys
  end

  # Return the set of edges in the graph.
  #
  # @yield [source, target] The source-target pair.
  # @yieldparam source [Object] The source node.
  # @yieldparam target [Object] The target node.
  # @return [Array] The edge pairs.
  #
  def edges &block
    if block
      @fwd.each { |s, tgt| tgt.keys.each { |t| block.call s, t } }
    else
      @fwd.map { |s, tgt| tgt.keys.map { |t| [s, t] } }.flatten 1
    end
  end

  # Returns the nodes ranked by their (outdegree - indegree).
  #
  # @yield [node, outdegree, indegree]
  # @return [Array] the ranked nodes
  #
  def nodes_by_degree &block
    # note sort turns a hash into an array of (key-value) pairs
    @nodes.sort do |a, b|
      al = a.last
      bl = b.last
      # compare first by descending degree difference
      cmp = bl[:outdegree] - bl[:indegree] <=> al[:outdegree] - al[:indegree]
      if cmp == 0 # tiebreaker conditions
        cmp = bl[:outdegree] <=> al[:outdegree] # descending outdegree
        cmp = al[:indegree]  <=> bl[:indegree] if cmp == 0 # ascending indegree
        cmp = a.first.to_s <=> b.first.to_s if cmp == 0 # ascending lexical
      end
      cmp
    end.map do |node, meta|
      block.call(node, *meta.values_at(:outdegree, :indegree)) if block
      n
    end
  end

  # Gets or sets the rank of the node.
  #
  # @param node [Object]
  # @param value [Integer,#to_i]
  # @return [Integer] the current or previous rank if a new one is set
  #
  def rank_for node, value = nil
    raise ArgumentError, "Graph does not contain #{node}" unless @nodes[node]

    return @nodes[node][:rank] ||= 0 unless value

    raise ArgumentError, "Value is not a non-negative integer" unless
      value.respond_to? :to_i and value.to_i >= 0

    old = @nodes[node][:rank]
    @nodes[node][:rank] = value.to_i

    old
  end

  # Determine if a node is a source (zero indegree).
  #
  # @return [true, false, nil] whether the node is a source or nil if the
  #  node is not present in the graph
  #
  def source? node
    return unless @nodes[node]
    @nodes[node][:indegree] == 0
  end

  # Determine if a node is a sink (zero outdegree).
  #
  # @return [true, false, nil] whether the node is a sink or nil if the
  #  node is not present in the graph
  #
  def sink? node
    return unless @nodes[node]
    @nodes[node][:outdegree] == 0
  end

  # Return the set of sources (zero-indegree) nodes. An optional block
  # affords iterating over the nodes.
  #
  # @yield [node] A block to iterate over source nodes
  # @yieldparam node [Object] A source node
  # @return [Array] The set of sources
  #
  def sources &block
    if block
      @nodes.each { |k, v| block.call k if v[:indegree] == 0 }
    else
      @nodes.select { |_, v| v[:indegree] == 0 }.map &:first
    end
  end

  # Return the set of sinks (zero-outdegree) nodes. An optional block
  # affords iterating over the nodes.
  #
  # @yield [node] A block to iterate over sink nodes
  # @yieldparam node [Object] A sink node
  # @return [Array] The set of sinks
  #
  def sinks &block
    if block
      @nodes.each { |k, v| block.call k if v[:outdegree] == 0 }
    else
      @nodes.select { |_, v| v[:outdegree] == 0 }.map &:first
    end
  end

  # Return the in-degree of the given node.
  #
  # @param node [Object] The node in question.
  # @return [Integer,nil] The in-degree, or nil if the node does not exist.
  #
  def indegree node
    return unless @nodes[node]
    @nodes[node][:indegree]
  end

  # Return the out-degree of the given node.
  #
  # @param node [Object] The node in question.
  # @return [Integer,nil] The out-degree, or nil if the node does not exist.
  #
  def outdegree node
    return unless @nodes[node]
    @nodes[node][:outdegree]
  end

  # Return the degree (in-degree + out-degree) of the given node.
  #
  # @param node [Object] The node in question.
  # @return [Integer,nil] The degree, or nil if the node does not exist.
  #
  def degree node
    return unless @nodes[node]
    @nodes[node][:outdegree] + @nodes[node][:indegree]
  end

  # Return the nodes adjacent on outbound edges to the given node or
  # nil if the node does not exist.
  #
  # @param node [Object]
  # @return [nil, Array]
  #
  def neighbours_out node
    return unless @nodes[node]
    @fwd.fetch(node, {}).keys
  end
  alias_method :neighbors_out, :neighbours_out

  # Return the nodes adjacent on inbound edges to the given node or
  # nil if the node does not exist.
  #
  # @param node [Object]
  # @return [nil, Array]
  #
  def neighbours_in node
    return unless @nodes[node]
    @rev.fetch(node, {}).keys
  end
  alias_method :neighbors_in, :neighbours_in

  # Return the intersection of adjacent nodes or nil if the node does
  # not exist.
  #
  # @param node [Object]
  # @return [nil, Array]
  #
  def neighbours node
    return unless @nodes[node]
    (neighbours_out(node) + neighbours_in(node)).uniq
  end
  alias_method :neighbors, :neighbours
  alias_method :incident, :neighbours

  # Return whether a node is present in the graph.
  #
  # @param node [Object]
  # @return [true,false]
  #
  def node? node
    !!@nodes[node]
  end

  # Return whether an edge is present in the graph.
  #
  # @param source [Object]
  # @param target [Object]
  # @return [true,false]
  #
  def edge? source, target
    @fwd.fetch(source, {}).key? target
  end

  # Add a node to the graph.
  #
  # @param node [Object] The node to add.
  # @return [Object]
  #
  def add node
    @nodes[node] ||= { indegree: 0, outdegree: 0 }

    node
  end

  # Remove a node (and adjacent edges) from the graph.
  #
  # @param node [Object] The node to remove.
  # @return [Object]
  #
  def remove node
    [[@rev, @fwd, :outdegree], [@fwd, @rev, :indegree]].each do |a, b, k|
      if a[node]
        a[node].each do |s|
          b[s].delete node          # remove the backreference
          b.delete s if b[s].empty? # get rid of the entry entirely (?)
          @nodes[s][k] -= 1         # decrement the degree
        end
      end
    end

    # now delete the node from each hash
    [@fwd, @rev, @nodes].each { |x| x.delete node }

    node
  end

  # Add an edge to the graph.
  #
  # @param source [Object] The source node
  # @param target [Object] The target node
  # @param weight [Numeric]
  # @return [Array,nil] The source-target pair or nil
  #  if the edge is already there.
  #
  def add_edge source, target, weight: 1
    raise ArgumentError, "Weight must be a number, not #{weight}" unless
      weight.is_a? Numeric

    @fwd[source] ||= {}
    @rev[target] ||= {}

    [source, target].each { |x| add x }

    # assume this is symmetric, that @rev[t] also does not include s
    return if @fwd[source].key? target

    @fwd[source][target] = @rev[target][source] = { weight: weight }

    @fwd[source] << target
    @rev[target] << source
    @nodes[source][:outdegree] += 1
    @nodes[target][:indegree]  += 1

    [source, target]
  end

  # Remove an edge from the graph.
  #
  # @param source [Object] The source node
  # @param target [Object] The target node
  # @return [Array,nil] The source-target pair or nil
  #  if the edge is not present.
  #
  def remove_edge source, target
    return unless @fwd[source].key? target

    @fwd[source].delete target
    @rev[target].delete source
    @nodes[source][:outdegree] -= 1
    @nodes[target][:indegree]  -= 1

    [source, target]
  end

  # Invert an edge.
  #
  # @param source [Object] The source node
  # @param target [Object] The target node
  # @return [Array,nil] The inverted source-target pair or nil
  #  if the original edge is not present.
  #
  def invert source, target
    add_edge target, source if remove_edge source, target
  end

  private

  # Sets an initial ranking. Don't run this if the graph has cycles.
  #
  # @return [Hash] The mapping of nodes to ranks
  #
  def initial_rank
    scanned = {} # { source => [target] }
    rank    = {} # { node => rank }
    # give us the nodes sorted by outdegree - indegree
    queue   = graph.nodes_by_degree

    # XXX THIS MAY LOOP FOREVER IF THE GRAPH HAS CYCLES
    until queue.empty?
      node = queue.shift # shift a node off the queue
      # test if inbound edges have been scanned
      nin  = neighbours_in node
      if nin.empty? or
          nin.reject { |n| scanned.fetch(n, []).include? node }.empty?
        # highest indegree rank plus one; defaults to zero
        rank_for node,
          rank[node] = (rank.values_at(*nin).compact.max || -1) + 1

        # record the outbound edges as having been scanned
        s = scanned[node] ||= Set.new
        s |= neighbours_out(node).to_set
      else
        # otherwise push the node back onto the end
        queue.push node
      end
    end

    # might as well return the hash?
    rank
  end

  def initial_cut_values
  end

  # Return all the (undirected) connected components in the graph.
  #
  # @return [Array<Set>] An array of nodes representing components.
  #
  def components
    pool = @nodes.keys.to_set
    out  = []

    until pool.empty?
      work  = Set.new           # initialize a working set;
      queue = [pool.to_a.first] # initialize a queue

      until queue.empty?
        node = queue.shift # get the node off the queue;
        work << node       # add node to working set

        # append new neighbours to queue
        queue += neighbours(node).reject { |n| work.include? n }
      end

      pool -= work # remove working set from pool;
      out  << work # add it to the list of connected components
    end
    
    out
  end

  # we just want this to return true if 
  def tight?
    # note if the graph is not fully connected this will fail
  end

  def slack source, target
    return unless @fwd.fetch(source, {})[target]
  end

  def feasible_tree
    initial_rank

    until tight?
      # 
    end

    initial_cut_values
  end

  def leave_edge
  end

  def enter_edge
  end

  def rank
    feasible_tree

    while (s, t = leave_edge)
      u, v = enter_edge s, t
      exchange [s, t], [u, v]
    end

    normalize

    balance
  end

  #def edge_weight source, target, value = nil
  #end

end
