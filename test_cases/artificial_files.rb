# Randomly generate 1 kilobyte blocks of data. We call those blocks 'tags' and
# associate each tag with a letter of the alphabet.

FILE_SIZE = 80

def random_kilobyte
  1000.times.map { rand(255).chr }.join
end

def random_file
  FILE_SIZE.times.map { random_kilobyte }.join
end

def overwrite_random_kilobytes(file, kilobyte)
  10.times do 
    index = rand(FILE_SIZE)
    file[index * 1000 ... (index + 1) * 1000] = kilobyte
  end
  file
end

tags = ("a".."e").each_with_object({}) do |letter, hash|
  hash[letter] = random_kilobyte
end

["a", "b", "c", "ab", "ac", "bc", "abc"].each do |identifier|
  file = random_file
  identifier.each_char do |letter|
    overwrite_random_kilobytes(file, tags[letter])
  end
  File.write("test_cases/artificial_files/#{identifier}", file)
end
