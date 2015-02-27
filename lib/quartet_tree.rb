require 'tree'
require 'byebug'

module Clustering
  class QuartetTree
    MAX_NUMBER_OF_ATTEMPTS = 2000

    attr_reader :tree
 
    # @param distance_matrix [DistanceMatrix]
    def initialize(distance_matrix)
      @distance_matrix = distance_matrix
      @tree = create_random_tree
    end

    # The clustering algorithm. Perform random permutations until a better
    # normalized benefit score cannot be found in a reasonable amount of attempts.
    def maximize_benefit_score
      best_score = normalized_benefit_score
      best_tree = @tree.detached_subtree_copy

      attempts = 0

      while attempts < MAX_NUMBER_OF_ATTEMPTS
        perform_mutation
        new_score = normalized_benefit_score
        if new_score > best_score
          best_score = new_score
          best_tree = @tree.detached_subtree_copy
          puts "new best score: #{best_score}."
          attempts = 0
        else
          attempts += 1
        end
      end

      @tree
    end

    private

    def create_random_tree
      labels = @distance_matrix.keys

      # Allows me to call leaves.next to add leaves in random order
      leaves = labels.shuffle.map { |label| Tree::TreeNode.new(label) }.each
      first_node = Tree::TreeNode.new("n0")
      # First node gets two leaves.
      first_node.add(leaves.next); first_node.add(leaves.next)

      random_tree = ( (1..labels.size - 3).reduce(first_node) do |tree, i|
        node = Tree::TreeNode.new("n#{i}")
        node.add(leaves.next)
        tree << node
      # last node gets an extra leaf: has two in total.
      end << leaves.next ).root
    end
    
    # (M - C{T}) / (M - m)
    def normalized_benefit_score
      # I calculate sum of maximal costs (M) and sum of minimal costs (m) every time now. 
      # Very naive, M and m should be cached.
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

    # A sequence of at least one but potentially many simple mutations, picked according to
    # the following distribution. First we pick the number k of simple mutations
    # (random leaf swap, random subtree swap or random subtree transfer) that we will
    # perform with probability 2^{-k}. For each such simple mutation, we choose randomly
    # between the three types.
    def perform_mutation
      3.times { random_leaf_swap } 
    end

    def random_leaf_swap
      byebug
      a, b = @tree.each_leaf.sample(2)
      @tree.swap_leaves(a, b)
    end

    def random_subtree_swap
    end

    def random_subtree_transfer
    end

    def groups_of_four
      @tree.each_leaf.combination(4)
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
      @distance_matrix[a][b] + @distance_matrix[c][d]
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
    
    # Gives the numbers of the internal nodes (n0, n1, ...) that must be
    # passed to travel from item a to item b.
    def nodes_passed(a, b)
      boundary_nodes = [ internal_node_number(a), internal_node_number(b) ].sort
      (boundary_nodes.first .. boundary_nodes.last).to_a
    end

    def internal_node_number(leaf)
      leaf.parent.name.match(/n(\d+)/).captures.first.to_i
    end
  end
end

class Tree::TreeNode
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
