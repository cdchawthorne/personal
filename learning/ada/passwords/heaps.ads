package heaps is
    type HeapNode is private;
    type MaxHeap is access HeapNode;

    procedure put(heap : MaxHeap);
    function empty_heap return MaxHeap;

    private type HeapNode is
        record
            boolean : is_empty;
            datum : integer;
            left_subtree : HeapPointer;
            right_subtree : HeapPointer;
        end record;
end heaps;
