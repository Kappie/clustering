require "zlib"
require "xz"

module Clustering
  module DistanceFunctions
    extend self

    NCD = lambda do |a, b|
      normalized_compression_distance(a, b)
    end

    def normalized_compression_distance(a, b)
      compressed_sizes = [ compressed_size(a), compressed_size(b) ]
      ( compressed_size(a + b) - compressed_sizes.min ).to_f / compressed_sizes.max
    end

    private

    def compressed_size(string)
      XZ.compress(string, compression_level = 9, check = :none, extreme = true).size
    end
  end
end
