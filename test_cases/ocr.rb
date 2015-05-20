require_relative "../lib/distance_matrix"
require_relative "../lib/normalized_compression_distance"

require "byebug"

CORPUS_LOCATION = "test_files/OCR/optdigits.tra"

# The original 32 x 32 bitmaps are split up into blocks of 8 x 8. Each block is 4 x 4.
# A character is represented by a 8 x 8 grid of (0 .. 16) integers. The number at a position counts
# the number of bits in the 4 x 4 block. The last integer of a line represents the digit (0 .. 9).
characters = File.readlines(CORPUS_LOCATION).map { |line| line.chomp.split(",") }

# Map to hex digits
characters = characters.map do |char|
  char.map { |block_count| block_count.to_i.to_s(17) }
end

# We drop the label (last element) now, since it's not part of the bitmap.
fours = characters.select { |char| char.last == "4" }.map { |char| char[0 ... -1] }.take(10) 
fives = characters.select { |char| char.last == "5" }.map { |char| char[0 ... -1] }.take(10)
sixes = characters.select { |char| char.last == "6" }.map { |char| char[0 ... -1] }.take(10)

objects = {}

[fours, fives, sixes].zip(["4", "5", "6"]).each do |characters, identifier|

  characters.each_with_index do |char, serial|
    label = identifier + "_" + serial.to_s
    objects[label] = char.join
  end

end

matrix = Clustering::DistanceMatrix.new(objects, Clustering::DistanceFunctions::NCD)
matrix.to_file("distance_matrices/fours_fives_sixes")
