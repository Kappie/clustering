require "byebug"

SOURCE_DIR = "test_files/literature"
TARGET_DIR = "test_files/literature-bits-expansion"

class Encoder
  def initialize(original)
    @original = original 
  end 
end

class DnaEncoder < Encoder
  ENCODING = {
    "g" => "00",
    "c" => "01",
    "t" => "10",
    "a" => "11"
  }

  def to_bytes
    @original.chars.each_slice(4).inject("") do |encoded_string, group_of_four|
      bitstring = group_of_four.map { |base| ENCODING[base] }.join
      character = bitstring.to_i(2).chr
      encoded_string += character
    end
  end
end

class UTF8Encoder < Encoder
  def expand_bytes
    @original.each_char.inject("") do |result, byte|
      bitstring = byte.ord.to_s(2).rjust(8, "0")
      puts "HELP" if bitstring.size > 8
      result += bitstring
    end
  end
end

Dir["#{SOURCE_DIR}/*"].each do |path|
  content = File.read(path)
  encoded_content = UTF8Encoder.new(content).expand_bytes

  File.write( File.join(TARGET_DIR, File.basename(path) ), encoded_content )

end
