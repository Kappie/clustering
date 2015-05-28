SOURCE_DIR = "test_files/source_code"
TARGET_DIR = "test_files/source_code_no_copyright"

LINES_TO_DROP = {
  ".rb" => 0,
  ".hs" => 12,
  ".clj" => 7
}


Dir[ File.join(SOURCE_DIR, "*") ].each do |path|
  content = File.read(path)
  extension = File.extname(path)

  cleaned_content = content.lines.drop( LINES_TO_DROP[extension] ).join

  target_path = File.join( TARGET_DIR, File.basename(path) )
  File.write(target_path, cleaned_content)
end
