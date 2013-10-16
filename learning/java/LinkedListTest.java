import LinkedList.LinkedList;
import java.math.BigInteger;

public class LinkedListTest {
    public static void main(String [] argv) {
        LinkedList l = new LinkedList();
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
