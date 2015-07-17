require_relative "spec_helper"

describe Clustering::DistanceMatrix do
  it 'accepts a hash with labels and objects and a distance function' do
    numbers = { one: 1, two: 2, three: 3 }
    add = lambda { |a, b| a + b }
    distance_matrix = Clustering::DistanceMatrix.new(numbers, add)
    result = {
      one:   { one: 2, two: 3, three: 4},
      two:   { one: 3, two: 4, three: 5},
      three: { one: 4, two: 5, three: 6}
    }
    expect(distance_matrix.distances).to eq result
  end

  it "has access to built-in distance functions" do
    expect(Clustering::DistanceMatrix.new({ string: "hoi" }, Clustering::DistanceFunctions::NCD).distances).to_not be_nil
  end
end
