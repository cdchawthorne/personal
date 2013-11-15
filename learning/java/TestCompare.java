class Comp implements java.util.Comparator<Integer> {
    @Override
    public int compare(Integer a, Integer b) {
        return b.compareTo(a);
        // if ((Integer)b < (Integer)a) {
            // return -1;
        // } else if ((Integer)b == (Integer)a) {
            // return 0;
        // } else {
            // return 1;
        // }
    }
}

class TestCompare {
    public static void main(String[] argv) {
        java.util.ArrayList<Integer> arr = new java.util.ArrayList<Integer>();
        arr.add(1);
        arr.add(2);
        arr.add(3);
        arr.add(4);
        arr.add(5);
        java.util.Collections.sort(arr, new Comp());
        System.out.println(arr);
    }
}
