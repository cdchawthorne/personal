import heapq
import itertools

class Block:
    def __init__(self, size, count):
        self.size = size
        self.count = count

    def __str__(self):
        return "({}, {})".format(self.size, self.count)

    def __repr__(self):
        return str(self)

class BlockList:
    # Apparently the heap library doesn't allow custom comparison operators
    class IndicesWrapper:
        def __init__(self, custom_lt, indices):
            self.indices = indices
            self.custom_lt = custom_lt

        def __lt__(self, other):
            return self.custom_lt(self, other)

    def __init__(self, blocks):
        self.blocks = sorted(blocks, key=lambda block: block.size,
                             reverse=True)
        self.num_blocks = len(self.blocks)

    def num_parents(self, index_tuple):
        return len([index for index in index_tuple if index != 0])

    def children(self, index_tuple):
        children = []
        for i in range(len(index_tuple)):
            if index_tuple[i] + 1 < self.num_blocks:
                new_tuple = (index_tuple[0:i] +
                             (index_tuple[i] + 1,) +
                             index_tuple[i+1:])
                children.append(new_tuple)

        return children

    def product_block(self, index_tuple):
        product_size = product_count = 1
        for i in index_tuple:
            product_size *= self.blocks[i].size
            product_count *= self.blocks[i].count

        return Block(product_size, product_count)

    def indices_wrapper_lt(self, first, second):
        first_product = self.product_block(first.indices)
        second_product = self.product_block(second.indices)
        return first_product.size > second_product.size

    def power(self, d):
        first_candidate = tuple(itertools.repeat(0, d))
        candidate_heap = [
                BlockList.IndicesWrapper(self.indices_wrapper_lt,
                                         first_candidate)
        ]
        # Dictionary mapping list of candidates to the number of parents we've
        # already processed
        partial_candidates = {}

        while candidate_heap:
            next_tuple = heapq.heappop(candidate_heap).indices
            yield self.product_block(next_tuple)

            for child in self.children(next_tuple):
                parents_processed = partial_candidates.get(child, 0) + 1
                if parents_processed == self.num_parents(child):
                    if parents_processed > 1:
                        del partial_candidates[child]
                    heapq.heappush(
                            candidate_heap,
                            BlockList.IndicesWrapper(self.indices_wrapper_lt,
                                                     child)
                    )
                else:
                    partial_candidates[child] = parents_processed

        assert not partial_candidates, "Partial candidates non-empty"
