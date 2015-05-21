require_relative "lib/normalized_compression_distance"
require_relative "lib/distance_matrix"
require_relative "lib/multiset"

PREFIX = "test_files"
DIR_NAME = "24-mammals-double-characters"

location = File.join(PREFIX, DIR_NAME)

#matrix = Clustering::DistanceMatrix.from_dir("test_files/#{DIR_NAME}", Clustering::DistanceFunctions::NCD)
#matrix.to_file("distance_matrices/#{DIR_NAME}")
#
#
#
#

matrix = Clustering::DistanceMatrix.from_dir(location, Clustering::DistanceFunctions::NCD)
matrix.to_file(File.join("distance_matrices", DIR_NAME))

#@multiset = Multiset.from_dir(DIR_NAME)

#puts @multiset.instance_eval { @objects.keys }
#puts
#puts @multiset.summary(2)
