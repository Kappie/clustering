# See Normalized Compression Distance of Multisets with Applications (Cohen, Vitanyi)

require "xz"
require "byebug"

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

  def summary(n)
    summary = {}
    objects_left = @objects.dup
    n.times do
      next_object = most_relevant_object(objects_left, summary)

      label, string = next_object
      summary[label] = string

      objects_left.delete(label)
    end
    summary.keys
  end

  def complexity
    compressed_size( concatenation(@objects.values) )
  end

  def complexity_without(label)
    compressed_size( concatenation_of_strings_except(@objects[label]) )
  end

  private

  def most_relevant_object(objects_left, summary)
    objects_left.max_by { |label, string| marginal_relevance(objects_left, summary, string) }
  end

  def marginal_relevance(objects_left, summary, string)
    labda = 0.75
    object_strings = objects_left.values
    summary_strings = summary.values

    labda * comprehensiveness(object_strings, string) + (1 - labda) * orthogonality(summary_strings, string)
  end

  # 1 - ( K(X) - K(X \ {x}) ) / K(x)
  def comprehensiveness(strings, string)
    1 - ( ( compressed_size( concatenation(strings) ) - compressed_size( concatenation( strings.reject { |str| str == string } ) ) ) / compressed_size(string).to_f )
  end

  # ( K(S \cup {x}) - K(S) ) / K(x)
  def orthogonality(strings, string)
    ( compressed_size( concatenation(strings) + string ) - compressed_size( concatenation(strings) ) ) / compressed_size(string).to_f
  end

  def concatenation_of_strings_except(string)
    strings = @objects.values
    strings.delete(string)
    concatenation(strings)
  end
  
  def concatenation(strings)
    strings.reduce(:+) || ""
  end

  def compressed_size(string)
    XZ.compress(string, compression_level = 9, check = :none, extreme = true).size
  end
end


