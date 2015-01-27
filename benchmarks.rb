require "benchmark"
require_relative "quartet_method"

six_item_tree = Clustering::QuartetTree.from_directory("6-mammals")
ten_item_tree = Clustering::QuartetTree.from_directory("10-mammals")

# "a" is append mode: append to end of file.
File.open("benchmarks.txt", "a") do |file|
  file.puts Time.now.strftime("Printed on %d %b at %I:%M%p")

  file.puts "tree of six items:"
  file.puts Benchmark.measure { six_item_tree.normalized_benefit_score }

  file.puts "tree of ten items:"
  file.puts Benchmark.measure { ten_item_tree.normalized_benefit_score }

  file.puts
end
