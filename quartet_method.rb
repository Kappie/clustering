# Implements quartet-method clustering.
# I use ruby-tree for tree structures.

require "tree"
require "zlib"
require "byebug"

module Clustering
  class QuartetTree < Tree::TreeNode
    COMPRESSION_LEVEL = 6
    NUMBER_OF_ATTEMPTS = 20

    def self.from_directory(dir_path)
      file_paths = Dir["#{dir_path}/*"]
      set = file_paths.each_with_object({}) do |path, result|
        result[ File.basename(path, ".txt") ] = File.read(path)
      end

      random_tree(set)
    end

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

    # The clustering algorithm. Perform random permutations until a better
    # normalized benefit score cannot be found in a reasonable amount of time.
    def maximize_benefit_score
      best_score = normalized_benefit_score

      attempts = 0

      while attempts < NUMBER_OF_ATTEMPTS
        perform_mutation
        new_score = normalized_benefit_score
        if new_score > best_score
          best_score = new_score
          puts "new best score: #{best_score}."
          attempts = 0
        else
          attempts += 1
        end
      end

      self
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

    # Hash of hashes of the normalized compression distances between files, like so:
    # { 
    #   "file1" => { "file1" => 1.0, "file2" => 0.68, ... },
    #   "file2" => { "file1" => 0.68", "file2" => 1.0, ...},
    #   ...
    # }
    # 
    # It is only calculated once.
    def distance_matrix
      @distance_matrix = @distance_matrix || each_leaf.each_with_object({}) do |row_leaf, matrix|
        matrix_row = each_leaf.each_with_object({}) do |col_leaf, row|
          row[col_leaf.name] = normalized_compression_distance(row_leaf.content, col_leaf.content)
        end
        matrix[row_leaf.name] = matrix_row
      end
    end

    # A sequence of at least one but potentially many simple mutations, picked according to
    # the following distribution. First we pick the number k of simple mutations
    # (random leaf swap, random subtree swap or random subtree transfer) that we will
    # perform with probability 2^{-k}. For each such simple mutation, we choose randomly
    # between the three types.
    def perform_mutation
      3.times { random_leaf_swap } 
    end

    def random_leaf_swap
      a, b = each_leaf.sample(2)
      swap_leaves(a, b)
    end

    def random_subtree_swap
    end

    def random_subtree_transfer
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

    def cost(topology)
      a, b  = topology.first.map { |leaf| leaf.name }
      c, d  = topology.last.map  { |leaf| leaf.name }
      distance_matrix[a][b] + distance_matrix[c][d]
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

class Tree::TreeNode
  # This method is in the documentation somehow, but not in the gem source code.
  def replace!(old_child, new_child)
    child_index = @children.find_index(old_child)

    old_child = remove! old_child
    add new_child, child_index

    old_child
  end

  # This method is in the documentation somehow, but not in the gem source code.
  def replace_with(node)
    @parent.replace!(self, node)
  end

  # Convenience methods
  def swap_leaves(a, b)
    old_a = a.detached_copy; old_b = b.detached_copy
    parent_a = a.parent; parent_b = b.parent

    # If parents are the same, TreeNode#add throws an error. So we do not swap at all.
    return a if parent_a == parent_b

    # After this, a is parentless and b has a's parent.
    a.replace_with(b) 

    parent_b.replace!(old_b, old_a)
  end
end
