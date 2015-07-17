require "tree"

class Tree::TreeNode
  def swap_leaves(a, b)
    old_a = a.detached_copy; old_b = b.detached_copy
    parent_a = a.parent; parent_b = b.parent

    # If parents are the same, TreeNode#add throws an error. So we do not swap at all.
    return a if parent_a == parent_b

    # After this, a is parentless and b has a's parent.
    a.replace_with(b) 

    parent_b.add(old_a, 0)
  end

  def swap_subtrees(a, b)
  end
end
