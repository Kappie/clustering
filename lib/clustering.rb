require_relative "distance_matrix"
require_relative "normalized_compression_distance"
require_relative "normalized_web_distance"
require_relative "quartet_tree"

module Clustering
  def self.cluster(objects)
    distances = DistanceMatrix.new(objects, DistanceFunctions::NCD).distances
    tree = QuartetTree.new(distances).maximize_benefit_score
    tree.print_tree 
    tree
  end

  def self.cluster_dir(source_dir)
    raise "Cannot find directory #{source_dir}." unless Dir.exists?(source_dir)
    objects = Dir[source_dir + "/*"].map { |path| [path, File.read(path)] }.to_h
    cluster(objects)
  end 

  def self.cluster_web(objects)
    distances = DistanceMatrix.new(objects, DistanceFunctions::NWD).distances
    p distances
    tree = QuartetTree.new(distances).maximize_benefit_score
    tree.print_tree 
  end
end
