public class Test {
    public static void main(String [] argv) {
        for (int i = 0, j = 5; i < 10 && j < 12; ++i, j = j+2) {
            System.out.println(i);
            System.out.println(j);
        }
    }
}
