"""
Detect a cycle in a linked list. Note that the head pointer may be 'None' if the list is empty.

A Node is defined as: 
 
    class Node(object):
        def __init__(self, data = None, next_node = None):
            self.data = data
            self.next = next_node
"""

def has_cycle(head, seen = None):
    if seen is None:
        seen = {}
    if head is None: 
        return False
    if head.data in seen:
        return True
    seen[head.data] = True
    return has_cycle(head.next, seen)