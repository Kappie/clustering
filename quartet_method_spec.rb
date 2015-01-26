require_relative "quartet_method"

describe "QuartetTree" do
  before(:each) do
    @n0 = Clustering::QuartetTree.new("n0")
    @n0 << Clustering::QuartetTree.new("item 1")
    @n0 << Clustering::QuartetTree.new("item 2")

    @kid = @n0 << Clustering::QuartetTree.new("n1") 
    @kid << Clustering::QuartetTree.new("item 3")
    @kid << Clustering::QuartetTree.new("item 4")
  end

  it "knows that 12|34 is consistent" do
    expect(@n0.consistent_with?(["item 1", "item 2"], ["item 3", "item 4"])).to eq true
  end
  
  it "knows that 13|24 is not consistent" do
    expect(@n0.consistent_with?(["item 1", "item 3"], ["item 2", "item 4"])).to eq false
  end
end
