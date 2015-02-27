require_relative "spec_helper"

describe Clustering::QuartetTree do
  it 'generates a random tree from a distance matrix, with the labels as leaf names' do
    tree = Clustering::QuartetTree.new(dummy_distances(4)).tree
    expect(tree.each_leaf.map { |leaf| leaf.name }).to contain_exactly("item 1", "item 2", "item 3", "item 4")
    expect(tree.map { |node| node.name }).to contain_exactly("item 1", "item 2", "item 3", "item 4", "n0", "n1")
  end
  
  it 'correctly clusters a tree with a trivial distance metric' do
    NUMBER_OF_ITEMS = 7

    @gauge_tree = Clustering::QuartetTree.new(dummy_distances(NUMBER_OF_ITEMS))

    def number_of_edges_passed(a, b)
      @gauge_tree.instance_eval { nodes_passed(a, b).size + 1 }
    end

    distance_metric = lambda do |a, b|
      a == b ? 0.0 : (number_of_edges_passed(a, b) + 1) / NUMBER_OF_ITEMS.to_f
    end

    objects = @gauge_tree.tree.each_leaf.map { |leaf| [leaf.name, leaf] }.to_h
    distances = Clustering::DistanceMatrix.new(objects, distance_metric).distances

    test_tree = Clustering::QuartetTree.new(distances) 

    test_tree.maximize_benefit_score

    test_tree.tree.print_tree
    @gauge_tree.tree.print_tree
  end
end

describe "Dummy distance function" do
  it "generates a matrix of the desired size" do
    result = {
      "item 1" => { "item 1" => "a distance", "item 2" => "a distance"},
      "item 2" => { "item 1" => "a distance", "item 2" => "a distance"}
    }
    expect(dummy_distances(2)).to eq result
  end
end

def dummy_distances(n)
  objects = (1..n).map { |i| ["item #{i}", "a value"] }.to_h
  function = lambda { |a, b| "a distance" }
  Clustering::DistanceMatrix.new(objects, function).distances
end


