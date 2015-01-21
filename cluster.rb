# I attempt to cluster string data by compression, as in the paper 'Clustering by Compression'
# by R. Cilibrasi and P. Vitanyi.
#
# I use the open source zlib library for compression, ai4r (AI for Ruby) library for clustering
# and datasets.

require "zlib"
require "ai4r"

# Debugging library.
require "byebug"

module Clustering
  class Cluster
    # 1 (worst) .. 9 (best)
    COMPRESSION_LEVEL = 6
    NUMBER_OF_CLUSTERS = 10

    attr_reader :clusters

    def initialize(root_path)
      paths = Dir["#{root_path}/*"]
      @file_names = paths.each_with_object({}) do |path, result|
        result[File.read(path)] = File.basename(path, ".txt")
      end
       
      # Data set expects data_items to be a two-dimensional array.
      data_items = @file_names.keys.map { |file| [file] }
      @data_set = Ai4r::Data::DataSet.new(data_items: data_items)

      cluster
    end

    private

    def cluster
      clusterer = Ai4r::Clusterers::KMeans.new 
      clusterer.distance_function = normalized_compression_distance
      clusterer.build(@data_set, NUMBER_OF_CLUSTERS)
      @clusters = clusterer.clusters.map do |data_set|
        data_set.data_items.map { |item| item.map { |string| @file_names[string] } }
      end
    end

    # This method builds a proc object to serve as the clusterer's distance function.
    # Note that a and b are data_items, i.e. arrays, so they need to be unpacked first.
    def normalized_compression_distance
      Proc.new do |a, b|
        a = a.first; b = b.first
        compressed_sizes = [ compressed_size(a), compressed_size(b) ]
        ( compressed_size(a + b) - compressed_sizes.min ).to_f / compressed_sizes.max
      end
    end

    def compressed_size(string)
      Zlib::Deflate.deflate(string, COMPRESSION_LEVEL).size
    end
  end
  
  class Clusterer
    def initialize(distance_matrix:, labels: [])
      @distance_matrix = distance_matrix
      @labels = labels
    end
  end

  class Dendrogram
  end
end
