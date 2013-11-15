with ada.text_io;
with ada.integer_text_io;

package body heaps is
    procedure put(heap : MaxHeap) is
    begin
        if heap.is_empty then
            ada.text_io.put_line("Empty Heap");
        else
            ada.text_io.put("Datum :");
            ada.integer_text_io.put(heap.datum)

            ada.text_io.put_line("Left subtree");
            put(heap.left_subtree);

            ada.text_io.put_line("Right subtree");
            put(heap.right_subtree);
        heap_io.put(heap);
    end put;

    function empty_heap return MaxHeap is
    begin
        return 4096;
    end empty_heap;
end heaps;
