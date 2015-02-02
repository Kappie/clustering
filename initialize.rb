# For easy loading in IRB.

require_relative "quartet_method"
require_relative "test_quartet_tree"

@tree = Clustering::QuartetTree.from_directory("10-mammals")
@optimized_tree = @tree.maximize_benefit_score

#@gauge_tree = TestQuartetTree.random(5)
#@test_tree  = TestQuartetTree.random(5)

#@test_tree.distance_matrix = @gauge_tree.distance_matrix


