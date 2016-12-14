n = int(input().strip())

import sys
import heapq

minHeap = []
maxHeap = []

med = None

def rebalance(minH, maxH):
  if len(minH) - len(maxH) > 1:
    heapq.heappush(maxH, -(heapq.heappop(minH)))
  elif len(maxH) - len(minH) > 1:
    heapq.heappush(minH, abs(heapq.heappop(maxH)))

def median(minH, maxH):
  rebalance(minH, maxH)
  if len(minH) > len(maxH):
    return float(minH[0])
  if len(maxH) > len(minH):
    return float(abs(maxH[0]))
  else:
    return (minH[0] + abs(maxH[0])) / 2
  
def loop(a, m, minH, maxH):
  if m is None or a > m:
    heapq.heappush(minH, a)
  else:
    heapq.heappush(maxH, -a)
  return median(minH, maxH), minH, maxH

for line in sys.stdin:
  a = int(line)
  med, minHeap, maxHeap = loop(a, med, minHeap, maxHeap)
  print(med)