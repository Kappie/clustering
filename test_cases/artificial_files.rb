require "securerandom"

# Randomly generate 1 kilobyte blocks of data. We call those blocks 'tags' and
# associate each tag with a letter of the alphabet.

FILE_SIZE = 8
TAG_SIZE = 1
NUMBER_OF_INSERTIONS = 1

def random_kilobyte
  # I have no idea why it generates twice as many random hex numbers as you tell it to.
  # Anyway, this generates a string with 1000 random hex numbers.
  SecureRandom.hex(500)
end

def random_file
  FILE_SIZE.times.map { random_kilobyte }.join
end

def overwrite_random_kilobytes(file, kilobyte)
  indices = (0 ... FILE_SIZE).to_a.sample(NUMBER_OF_INSERTIONS)
  indices.each do |index|
    file[index * 1000 ... (index + 1) * 1000] = kilobyte
  end
  file
end

tags = ["a", "b", "c", "x", "y", "z"].each_with_object({}) do |letter, hash|
  hash[letter] = random_kilobyte
end

words = ["a", "b", "c", "ab", "ac", "bc", "abc", "x", "y", "z", "xy", "xz", "yz", "xyz", "abcx"]

words.each do |word|
  file = random_file
  word.each_char do |letter|
    overwrite_random_kilobytes(file, tags[letter])
  end
  File.write("test_files/artificial_files_3/#{word}.txt", file)
end

