# A few remarks:
#
# The calculation of the normalized benefit score is untested.
# The calculation of the normalized compression distance is untested.
# I will have to think of a way of testing those things.

require_relative "quartet_method"
require_relative "test_quartet_tree"

describe "QuartetTree" do
  before(:each) do
    @n0 = Clustering::QuartetTree.new("n0")
    @leaf1 = Clustering::QuartetTree.new("item 1")
    @leaf2 = Clustering::QuartetTree.new("item 2")
    @leaf3 = Clustering::QuartetTree.new("item 3")
    @leaf4 = Clustering::QuartetTree.new("item 4")

    @n0 << @leaf1
    @n0 << @leaf2

    @n1 = @n0 << Clustering::QuartetTree.new("n1") 
    @n1 << @leaf3
    @n1 << @leaf4

    @sample_input = {
      "Tangled Up In Blue" => "0010101010100000",
      "You're A Big Girl Now" => "010101010101010",
      "Buckets Of Rain" => "1110110101011101",
      "Shelter From The Storm" => "010101011111010101"
    }
  end

  it "knows that 12|34 is consistent" do
    expect(@n0.consistent_with?([[@leaf1, @leaf2], [@leaf3, @leaf4]])).to eq true
  end
  
  it "knows that 13|24 is not consistent" do
    expect(@n0.consistent_with?([[@leaf1, @leaf3], [@leaf2, @leaf4]])).to eq false
  end

  it "selects 12|34| as the consistent topology" do
    expect(@n0.consistent_topology([@leaf1, @leaf2, @leaf3, @leaf4])).to eq [[@leaf1, @leaf2], [@leaf3, @leaf4]]
  end

  it "always has exactly one consistent topology for every group of four items" do
    10.times do
      tree = Clustering::QuartetTree.random_tree(random_set) 
      tree.groups_of_four.each do |group_of_four|
        expect( tree.topologies(group_of_four).select { |topology| tree.consistent_with?(topology) }.size ).to eq 1
      end
    end
  end

  it "always has a normalized benefit score between 0 and 1" do
    10.times do
      tree = Clustering::QuartetTree.random_tree(random_set)
      expect(tree.normalized_benefit_score).to be >= 0.0
      expect(tree.normalized_benefit_score).to be <= 1.0
    end
  end

  it "returns a QuartetTree when doing a detached copy" do
    expect(@n0.detached_subtree_copy.class).to eq Clustering::QuartetTree
  end

  # See page 11 of the Clustering by Compression paper.
  it "correctly clusters a tree with a trivial distance metric" do
    NUMBER_OF_ITEMS = 7
    gauge_tree = TestQuartetTree.random(NUMBER_OF_ITEMS)
    test_tree = TestQuartetTree.random(NUMBER_OF_ITEMS)
    test_tree.distance_matrix = gauge_tree.distance_matrix
    expect(test_tree.normalized_benefit_score).to be < 1.0

    optimized_tree = test_tree.maximize_benefit_score
    optimized_tree.distance_matrix = gauge_tree.distance_matrix
    expect(optimized_tree.normalized_benefit_score).to eq 1.0
  end
end

describe "Tree::TreeNode" do
  before(:each) do
    @root = Tree::TreeNode.new("root")
    @internal_node = Tree::TreeNode.new("internal node")
    @leaf1 = Tree::TreeNode.new("leaf 1")
    @leaf2 = Tree::TreeNode.new("leaf 2")
    @leaf3 = Tree::TreeNode.new("leaf 3")
    @leaf4 = Tree::TreeNode.new("leaf 4")

    @root << @internal_node
    @root << @leaf1
    @root << @leaf2

    @internal_node << @leaf3
    @internal_node << @leaf4
  end

  it "swaps leaves" do
    @root.swap_leaves(@leaf1, @leaf3)
    expect(@root[1].name).to eq "leaf 3"
    expect(@internal_node[0].name).to eq "leaf 1"
  end
end

def random_set
  length = 4 + rand(6)
  (0 .. length).map { |x| ["item #{x}", random_binary_string] }.to_h
end

def random_binary_string
  (0 .. rand(50)).map { rand < 0.5 ? "0" : "1" }.join
end

