# Implements quartet-method clustering.
# I use ruby-tree for tree structures.

require "tree"
require "zlib"
require "byebug"

module Clustering
  class QuartetTree < Tree::TreeNode
    # Construct tree with n leaf nodes, containing the n items, and
    # n - 2 internal nodes, labeled with a_{0} through a_{n-3}.
    def self.random_tree(items)
      # Allows me to call leaves.next to add leaves in random order
      leaves = items.shuffle.map { |item| QuartetTree.new(item) }.each
      first_node = QuartetTree.new("n0")
      # First node gets two leaves.
      first_node.add(leaves.next); first_node.add(leaves.next)

      random_tree = ( (1..items.size - 3).reduce(first_node) do |tree, i|
        node = QuartetTree.new("n#{i}")
        node.add(leaves.next)
        tree << node
      # last node gets an extra leaf: has two in total.
      end << leaves.next ).root
    end

    def consistent_with?(first_pair, second_pair)
      # & is set intersection.
      ( nodes_passed(*first_pair) & nodes_passed(*second_pair) ).empty?
    end

    # (M - C{T}) / (M - m)
    def normalized_benefit_score
    end

    # C{T}
    def total_cost

    end

    # TODO: something with content
    def cost(first_pair, second_pair)
      normalized_compression_distance(*first_pair) + normalized_compression_distance(*second_pair)
    end

    def quartet_topologies
      each_leaf.map { |leaf| leaf.name }.combination(4) { |group_of_four| topologies(group_of_four) }
    end

    def topologies(set)
      [
        [[ set[0], set[1] ], [ set[2], set[3] ]],
        [[ set[0], set[2] ], [ set[1], set[3] ]],
        [[ set[0], set[3] ], [ set[1], set[2] ]]
      ] 
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
      find_by_name(item).parent.name
    end

    # Gives the numbers of the internal nodes (n0, n1, ...) that must be
    # passed to travel from item a to item b.
    def nodes_passed(a, b)
      boundary_nodes = [ parent_name(a)[-1].to_i, parent_name(b)[-1].to_i ].sort
      (boundary_nodes.first .. boundary_nodes.last).to_a
    end
  end
end
