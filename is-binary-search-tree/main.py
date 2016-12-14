""" Node is defined as
class node:
    def __init__(self, data):
        self.data = data
        self.left = None
        self.right = None
"""

def check_binary_search_tree_(root, minBound = 0, maxBound = 10000):

  if root.left is not None:
    if root.left.data >= root.data:
      return False
    if not minBound <= root.left.data <= maxBound:
      return False
    if not check_binary_search_tree_(root.left, minBound, root.data - 1):
      return False
    
  if root.right is not None:
    if root.right.data <= root.data:
      return False
    if not minBound <= root.right.data <= maxBound:
      return False
    if not check_binary_search_tree_(root.right, root.data + 1, maxBound):
      return False
    
  return True