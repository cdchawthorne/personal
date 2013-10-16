package LinkedList;

class Node {
    private Object datum;
    private Node next;
    private boolean hasNext = false;

    public Object getDatum() {
        return datum;
    }
    public void setDatum(Object newDatum) {
        datum = newDatum;
    }

    public boolean hasNext() {
        return hasNext;
    }
    public Node getNext() throws LinkedListBoundsError {
        if (!hasNext) {
            throw new LinkedListBoundsError();
        }
        return next;
    }
    public void setNext(Node newNext) {
        next = newNext;
        hasNext = true;
    }

    public Node(Object obj) {
        datum = obj;
    }

}
