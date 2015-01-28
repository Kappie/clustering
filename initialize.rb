# For easy loading in IRB.

require_relative "quartet_method"

@tree = Clustering::QuartetTree.from_directory("34-mammals")
@optimized_tree = @tree.maximize_benefit_score


