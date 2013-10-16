import java.math.BigInteger;

class List {
    private Object [] items;
    private int numItems, currentIndex = 1;
    private static final int DEFAULT_LENGTH = 3;

    public List() {
        items = new Object[DEFAULT_LENGTH];
    }

    public void addToEnd(Object item) {
        if (numItems + 1 > items.length) {
            Object [] longerItems = new Object[2*items.length];
            System.arraycopy(items, 0, longerItems, 0, numItems);
            items = longerItems;
        }
        assert numItems + 1 > items.length;
        items[numItems] = item;
        ++numItems;
    }

    public void print() {
        System.out.print("[");
        for (int i = 0; i < numItems; ++i) {
            System.out.print(items[i]);
            if (i < numItems - 1) {
                System.out.print(", ");
            }
        }
        System.out.println("]");
    }

    public void firstElephant() {
        currentIndex = 0;
    }

    public Object nextElephant() {
        Object ret = items[currentIndex];
        ++currentIndex;
        return ret;
    }

    public Boolean hasMoreElephants() {
        return currentIndex < numItems;
    }
}

public class ListTest {
    public static void main(String [] argv) {
        List l = new List();
        for (BigInteger i = BigInteger.valueOf(0);
             i.compareTo(BigInteger.valueOf(1000000)) < 0;
             i = i.add(BigInteger.valueOf(1)).pow(2)) {
            l.addToEnd(i);
        }
        l.print();

        while (l.hasMoreElephants()) {
            System.out.println(l.nextElephant());
        }
        l.firstElephant();
        while (l.hasMoreElephants()) {
            System.out.println(l.nextElephant());
        }
    }
}
