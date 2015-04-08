# See Normalized Compression Distance of Multisets with Applications (Cohen, Vitanyi)

require "xz"

class Multiset
  def self.from_dir(path)
    paths = Dir["#{path}/*"]
    objects = paths.each_with_object({}) do |path, result|
      label = File.basename(path, ".txt")
      result[label] = File.read(path)
    end

    new(objects)
  end

  def initialize(objects)
    @objects = objects
  end

  # E_min (X) = min { G(X) - G(x_1), ... , G(X) - G(x_n) } = G(X) - max { G(x) }
  # E_min can be interpreted as the length of the length of the program that
  # outputs the most comprehensive object that contains the most information
  # about all the other objects of X.
  # We want Arg min { G(X) - G(x_1), ... , G(X) - G(x_n) }
  #def most_comprehensive_object
    #compressed_size_of_set = compressed_size(concatenation_of_objects) 
    ## { "file1" => "01010101010" }.min_by { |label, string| string.size } geeft ["file1", "01010101010"]
    ## Hier willen we alleen het label terug hebben (vandaar de .first aan het eind.)

    #@objects.each { |label, string| puts "#{label}: #{compressed_size(string)}" }
    #@objects.max_by { |label, string| compressed_size_of_set - compressed_size(string) }.first
  #end
  
  def most_comprehensive_object


  end

  # Benadering van K(X|a) door G(X|a), waar X de multiset en a een element uit X.
  # G compressor.
  #def complexity(x)
    
  #end
  #

  def concatenation_of_objects_except(object)
    objects = @objects.dup.values
    objects.delete(object)

    objects
    
  end

  def compressed_size(string)
    XZ.compress(string, compression_level = 9, check = :none, extreme = true).size
  end

  def concatenation_of_objects
    strings.reduce(:+)
  end

  def strings
    @objects.values
  end
end


