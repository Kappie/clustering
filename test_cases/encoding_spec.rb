require_relative "encoding.rb"

describe "DnaEncoder" do
  it "encodes a string correctly" do
    expect(DnaEncoder.new("gatcaacg").to_bytes).to eq "9\xF4".b
  end
end

describe "UTF8Encoder" do
  it 'expands bytes correctly' do
    expect(UTF8Encoder.new("hoi").expand_bytes).to eq "011010000110111101101001"
  end
end
