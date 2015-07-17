PREFIX = "test_files"
SOURCE_DIR = "24-mammals"
TARGET_DIR = "24-mammals-double-characters"

source_location = File.join(PREFIX, SOURCE_DIR)
target_location = File.join(PREFIX, TARGET_DIR)

class String
  # "hoi".double_characters # => "hhooii"
  def double_characters
    self.chars.inject("") { |result, char| result += char * 2 }
  end
end

Dir[ File.join(source_location, "*") ].each do |path|
  contents = File.read(path)
  File.write( File.join( target_location, File.basename(path) ), contents.double_characters )
end

