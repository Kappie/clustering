module Clustering
  # A class that calculates distances between objects given a distance function.
  # @example
  #   numbers = { one: 1, two: 2}
  #   add     = lambda { |a, b| a + b }
  #   matrix = DistanceMatrix.new(numbers, add)
  #   matrix.distances #=> { one: { one: 2, two: 3}, two: { one: 3, two: 4} }
  class DistanceMatrix
    attr_reader :distances

    # @param objects [Hash]
    #   A hash with symbols (labels of the objects) and objects.
    # @param distance_function [Proc]
    #   A lambda that takes two objects and returns a distance.
    def initialize(objects, distance_function)
      @objects = objects
      @distance_function = distance_function  
      calculate_distances
    end

    # @param path
    #   Location to which distance matrix will be written.
    def to_file(path)
      File.open(path, "w") do |file|
        @distances.each do |label_x, row|
          file.write(label_x)
          file.write(" ")
          row.each do |label_y, distance|
            file.write(distance)
            file.write(" ")
          end
          file.puts
        end 
      end
    end

    private

    def calculate_distances
      @distances = @objects.each_with_object({}) do |(row_label, row_element), matrix|
        matrix_row = @objects.each_with_object({}) do |(column_label, column_element), row|
          row[column_label] = @distance_function.call(row_element, column_element)
        end

        matrix[row_label] = matrix_row
      end
    end
  end
  
end
