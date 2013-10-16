class Foo {
    public int x = 3;
}

class TestDefaults {
    public static void main(String [] argv) {
        Foo a = new Foo();
        a.x = 4;
        Foo b = new Foo();
        System.out.println(String.format("a.x is %d, b.x is %d", a.x, b.x));
    }
}
