require_relative "distance_matrix"
require_relative "normalized_compression_distance"
require_relative "quartet_tree"

module Clustering
  def self.cluster(source_dir)
    raise "Cannot find directory #{source_dir}." unless Dir.exists?(source_dir)

    objects = Dir[source_dir + "/*"].map { |path| [path, File.read(path)] }.to_h
    distances = DistanceMatrix.new(objects, DistanceFunctions::NCD).distances
    tree = QuartetTree.new(distances).maximize_benefit_score
    tree.print_tree 
    tree
  end 
end
