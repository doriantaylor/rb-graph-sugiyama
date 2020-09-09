require 'graph/sugiyama/version'

require 'set'

# This is a basic little unlabeled directed graph implementation for
# use with the Sugiyama framework
class Graph::Implementation

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

  def initialize_copy orig
    warn 'dupped'
  end

  def nodes
    @nodes.keys
  end

  def indegree node
    return unless @nodes[node]
    @nodes[node][:indegree]
  end

  def outdegree node
    return unless @nodes[node]
    @nodes[node][:outdegree]
  end

  def degree node
    return unless @nodes[node]
    @nodes[node][:outdegree] + @nodes[node][:indegree]
  end

  def add node
    @nodes[node] ||= { indegree: 0, outdegree: 0 }

    node
  end

  def remove node
    [[@rev, @fwd, :outdegree], [@fwd, @rev, :indegree]].each do |a, b, k|
      if a[node]
        a[node].each do |s|
          b[s].delete? node  # remove the backreference
          b.delete s if b[s].empty?
          @nodes[s][k] -= 1  # decrement the degree
        end
      end
    end

    # now delete the node from each hash
    [@fwd, @rev, @nodes].each { |x| x.delete node }

    node
  end

  def add_edge s, t
    @fwd[s] ||= Set.new
    @rev[t] ||= Set.new

    [s, t].each { |x| add x }

    # assume this is symmetric, that @rev[t] also does not include s
    return if @fwd[s].include? t

    @fwd[s] << t
    @rev[t] << s
    @nodes[s][:outdegree] += 1
    @nodes[t][:indegree]  += 1

    [s, t]
  end

  def remove_edge s, t
    return unless @fwd[s].include? t

    @fwd[s].delete t
    @rev[t].delete s
    @nodes[s][:outdegree] -= 1
    @nodes[t][:indegree]  -= 1

    [s, t]
  end

  def invert s, t
    add_edge t, s if remove_edge s, t
  end
end
