require_relative "lib/normalized_compression_distance"
require_relative "lib/distance_matrix"


def objects_from_dir(path)
  
  paths = Dir["#{path}/*"]
  paths.each_with_object({}) do |path, objects|
    objects[File.basename(path, ".txt")] = File.read(path)
  end
  
end

@matrix = Clustering::DistanceMatrix.new(objects_from_dir("test_files/34-mammals"), Clustering::DistanceFunctions::NCD)
@matrix.to_file("./matrix_34_mammals")
