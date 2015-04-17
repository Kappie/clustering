require_relative "lib/normalized_compression_distance"
require_relative "lib/distance_matrix"
require_relative "lib/multiset"

DIR_NAME = "test_files/artificial_files_3"

#matrix = Clustering::DistanceMatrix.from_dir("test_files/#{DIR_NAME}", Clustering::DistanceFunctions::NCD)
#matrix.to_file("distance_matrices/#{DIR_NAME}")
#
#
#
#

#matrix = Clustering::DistanceMatrix.from_dir(DIR_NAME, Clustering::DistanceFunctions::NCD)
#matrix.to_file("distance_matrices/34-mammals-xz")

@multiset = Multiset.from_dir(DIR_NAME)

puts @multiset.instance_eval { @objects.keys }
puts
puts @multiset.summary(2)
