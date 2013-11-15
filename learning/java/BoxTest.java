class Box<T1> {
    private T1 val;

    Box(T1 val) {
        this.val = val;
    }

    T1 getVal() {
        return val;
    }

    void setVal(T1 val) {
        this.val = val;
    }
}

public class BoxTest {
    public static void main(String [] argv) {
        java.util.ArrayList<int> foo = new java.util.ArrayList<int>(5);
        foo.add(3);
        foo.add(4);
        System.out.println(foo);
    }
}
