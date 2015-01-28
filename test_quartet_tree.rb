# Quartet tree with a trivial distance metrix, to test the clustering algorithm,
# as in the page 11 of the Clustering by Compression paper.
class TestQuartetTree < Clustering::QuartetTree
  def self.random(n)
    set = (1..n).map(&:to_s).each_with_object({}) do |x, result|
      result[x] = ""
    end
    random_tree(set)
  end

  def distance_matrix=(distance_matrix)
    @distance_matrix = distance_matrix
  end

  def normalized_compression_distance(a, b)
    a == b ? 0.0 : (number_of_edges_passed(a, b) + 1) / 18.0
  end

  def number_of_edges_passed(a, b)
    nodes_passed(a, b).size + 1
  end
end
