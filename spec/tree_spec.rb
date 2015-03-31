require "spec_helper"

describe "TreeNode" do
  xit "swaps subtrees" do
    n0 = TreeNode.new("n0")
    n1 = n0 << TreeNode.new("n1")
   
    n0 << TreeNode.new("item 1")
    n0 << TreeNode.new("item 2")

    n1 << TreeNode.new("item 3")
    n1 << TreeNode.new("item 4")
    
    n0.swap_subtrees(n0, n1)
  end
end
