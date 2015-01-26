# Implements quartet-method clustering.
# I use ruby-tree for tree structures.

require "tree"
require "zlib"
require "byebug"

module Clustering
  class QuartetTree < Tree::TreeNode
    COMPRESSION_LEVEL = 6

    # Construct tree with n leaf nodes, containing the n items, and
    # n - 2 internal nodes, labeled with a_{0} through a_{n-3}.
    # Accepts a hash with labels as keys and contents as values, as in:
    #
    # set = {
    #   "Tangled Up In Blue" => "010100011111010...",
    #   "You're a Big Girl Now" => "01010101010000...",
    #   "If You See Her, Say Hello" => "01010100..."
    # }
    def self.random_tree(set)
      # Allows me to call leaves.next to add leaves in random order
      leaves = set.to_a.shuffle.map { |label, content| QuartetTree.new(label, content) }.each
      first_node = QuartetTree.new("n0")
      # First node gets two leaves.
      first_node.add(leaves.next); first_node.add(leaves.next)

      random_tree = ( (1..set.size - 3).reduce(first_node) do |tree, i|
        node = QuartetTree.new("n#{i}")
        node.add(leaves.next)
        tree << node
      # last node gets an extra leaf: has two in total.
      end << leaves.next ).root
    end

    # Exactly one topology for a given group of four is consistent with any given tree.
    # This method returns the consistent topology, e.g. [[1, 2], [3, 4]],
    # meaning 12|34.
    def consistent_topology(group_of_four)
      topologies(group_of_four).detect { |topology| consistent_with?(topology) }
    end

    def consistent_with?(topology)
      # & is set intersection.
      ( nodes_passed(*topology.first) & nodes_passed(*topology.last) ).empty?
    end

    # (M - C{T}) / (M - m)
    def normalized_benefit_score
      sum_of_maximal_costs = 0
      sum_of_minimal_costs = 0
      total_tree_cost = 0

      groups_of_four.each do |group_of_four|
        costs = costs_of_topologies(group_of_four)
        sum_of_maximal_costs += costs.max
        sum_of_minimal_costs += costs.min
        
        # I calculate the cost twice now. Not so nice.
        total_tree_cost += cost( consistent_topology(group_of_four) )
      end

      (sum_of_maximal_costs - total_tree_cost) / (sum_of_maximal_costs - sum_of_minimal_costs)
    end

    def cost(topology)
      first_pair  = topology.first.map { |leaf| leaf.content }
      second_pair = topology.last.map  { |leaf| leaf.content }
      normalized_compression_distance(*first_pair) + normalized_compression_distance(*second_pair)
    end

    # Gives the three possible topologies for every combination of four labels
    def groups_of_four
      each_leaf.combination(4)
    end

    def topologies(set)
      [
        [[ set[0], set[1] ], [ set[2], set[3] ]],
        [[ set[0], set[2] ], [ set[1], set[3] ]],
        [[ set[0], set[3] ], [ set[1], set[2] ]]
      ] 
    end

    # Gives the costs of the three different topologies for a group of four leaves.
    def costs_of_topologies(group_of_four)
      topologies(group_of_four).map { |topology| cost(topology) }
    end

    private

    def normalized_compression_distance(a, b)
      compressed_sizes = [ compressed_size(a), compressed_size(b) ]
      ( compressed_size(a + b) - compressed_sizes.min ).to_f / compressed_sizes.max
    end

    def compressed_size(string)
      Zlib::Deflate.deflate(string, COMPRESSION_LEVEL).size
    end

    def find_by_name(name)
      find { |node| node.name == name }
    end

    def parent_name(item)
      item.parent.name
    end

    # Gives the numbers of the internal nodes (n0, n1, ...) that must be
    # passed to travel from item a to item b.
    def nodes_passed(a, b)
      boundary_nodes = [ parent_name(a)[-1].to_i, parent_name(b)[-1].to_i ].sort
      (boundary_nodes.first .. boundary_nodes.last).to_a
    end
  end
end
