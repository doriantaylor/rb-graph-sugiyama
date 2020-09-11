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
      @fwd.each { |s, tgt| tgt.each { |t| block.call s, t } }
    else
      @fwd.map { |s, tgt| tgt.map { |t| [s, t] } }.flatten 1
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
  # @return [Array,nil] The source-target pair or nil
  #  if the edge is already there.
  #
  def add_edge source, target
    @fwd[source] ||= Set.new
    @rev[target] ||= Set.new

    [source, target].each { |x| add x }

    # assume this is symmetric, that @rev[t] also does not include s
    return if @fwd[source].include? target

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
    return unless @fwd[source].include? target

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
end
