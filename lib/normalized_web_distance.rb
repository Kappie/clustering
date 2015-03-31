require "ruby-web-search"
require "byebug"

module Clustering
  module DistanceFunctions
    NWD = lambda do |a, b|
      normalized_web_distance(a, b)
    end

    def normalized_web_distance(a, b)
      scale_factor = 1_000_000
      results_a = RubyWebSearch::Google.search(query: a).estimated_result_count.to_i
      results_b = RubyWebSearch::Google.search(query: b).estimated_result_count.to_i
      combined_results = RubyWebSearch::Google.search(query: "{a} {b}").estimated_result_count.to_i
      byebug
      ( [Math.log(results_a), Math.log(results_b)].max - Math.log(combined_results) ) / ( Math.log(scale_factor) - [Math.log(results_a), Math.log(results_b)].min )
    end
  end
end
