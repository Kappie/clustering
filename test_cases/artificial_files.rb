require "securerandom"

# Randomly generate 1 kilobyte blocks of data. We call those blocks 'tags' and
# associate each tag with a letter of the alphabet.

FILE_SIZE = 80
TAG_SIZE = 1
NUMBER_OF_INSERTIONS = 10

def random_kilobyte
  # I have no idea why it generates twice as many random hex numbers as you tell it to.
  # Anyway, this generates a string with 1000 random hex numbers.
  #SecureRandom.hex(500)
  
  # Now I use characters 0 - 9 and a - z.
  # 1000.times.map { rand(36).to_s(36) }.join

  SecureRandom.random_bytes(1000)
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

tags = ("a".."k").each_with_object({}) do |letter, hash|
  hash[letter] = random_kilobyte
end

words = ["abc", "abcd", "abce", "abe", "ab", "acfg", "abfg", "abhi", "abhj", "ac", "a", "c", "e", "jk", "hijk", "ij", "j", "i", "f", "h", "g", "d"]

#words.each do |word|
  #file = random_file
  #word.each_char do |letter|
    #overwrite_random_kilobytes(file, tags[letter])
  #end
  #File.write("test_files/artificial_files_rudi/#{word}.txt", file)
#end

34.times do |index|
  File.write("test_files/random_files/file#{index}.txt", random_file)
end



