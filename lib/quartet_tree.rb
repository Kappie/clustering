# I need bundler/setup because I take the 'tree' gem from a github repository (See Gemfile.)
require 'bundler/setup'
require 'tree'

module Clustering
  class QuartetTree < Tree::TreeNode
 
    # @param distance_matrix [DistanceMatrix]
    def initialize(distance_matrix)
      
    end
  end
end
