from blocks import *

def get_power(size_count_pairs, d):
    return list(BlockList([Block(*pair) for pair in size_count_pairs]).power(d))

p = get_power([[10,1], [2,1], [1,1]], 3)
print(p)
