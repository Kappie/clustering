require "byebug"

SOURCE_DIR = "test_files/24-mammals"
TARGET_DIR = "test_files/24-mammals-2bit"

ENCODING = {
  "g" => "00",
  "c" => "01",
  "t" => "10",
  "a" => "11"
}

def encode(dna_sequence)
  dna_sequence.chars.each_slice(4).inject("") do |encoded_string, group_of_four|
    bitstring = group_of_four.map { |base| ENCODING[base] }.join
    character = bitstring.to_i(2).chr
    encoded_string += character
  end
end

Dir["#{SOURCE_DIR}/*"].each do |path|
  content = File.read(path)
  encoded_content = encode(content)

  File.write( File.join(TARGET_DIR, File.basename(path) ), encoded_content )

end
