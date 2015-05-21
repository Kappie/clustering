require_relative "encoding.rb"

describe "encode" do
  it "encodes a string correctly" do
    expect(encode("gatcaacg")).to eq "9\xF4".b
  end
end
