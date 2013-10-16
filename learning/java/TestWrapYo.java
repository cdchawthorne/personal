class ObjectHolder {
    private Object foo;

    public ObjectHolder(Object foo) {
        this.foo = foo;
    }

    public void printFoo() {
        System.out.println(foo);
    }
    public Object getFoo() {
        return foo;
    }
}

public class TestWrapYo {
    public static void main(String[] argv) {
        ObjectHolder L = new ObjectHolder("s");
        Integer y = (Integer)(L.getFoo());
        System.out.println(y);
    }
}
