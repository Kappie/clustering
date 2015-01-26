# A few remarks:
#
# The calculation of the normalized benefit score is untested.
# The calculation of the normalized compression distance is untested.
# I will have to think of a way of testing those things.

require_relative "quartet_method"

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

  it "should calculate some kind of normalized benefit score" do
    input = {
      "Tangled Up In Blue" => "0010101010100000",
      "You're A Big Girl Now" => "010101010101010",
      "Buckets Of Rain" => "1110110101011101",
      "Shelter From The Storm" => "010101011111010101"
    }
    tree = Clustering::QuartetTree.random_tree(input)
    expect(tree.normalized_benefit_score).to be >= 0.0
  end

end

def random_set
  length = 4 + rand(6)
  (0 .. length).map { |x| "item #{x}" }
end
