package LinkedList;
import java.math.BigInteger;

public class LinkedList {
    private Node start, end, current;
    private boolean hasElephants = false;

    public void addToEnd(Object item) {
        Node newNode = new Node(item);
        if (!hasElephants) {
            start = end = current = newNode;
            hasElephants = true;
        } else {
            end.setNext(newNode);
            end = newNode;
        }
    }

    public void print() {
        System.out.print("[");

        Node iter = start;
        while (true) {
            System.out.print(iter.getDatum());
            try {
                iter = iter.getNext();
            } catch (LinkedListBoundsError e) {
                break;
            }
            System.out.print(", ");
        }

        System.out.println("]");
    }

    public void firstElephant() {
        current = start;
    }

    public Object nextElephant() {
        Object ret = current.getDatum();
        try {
            current = current.getNext();
        } catch (LinkedListBoundsError e) {
            current = null;
        }
        return ret;
    }

    public boolean hasMoreElephants() {
        return current != null;
    }
}
