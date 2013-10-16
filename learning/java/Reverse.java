public class Reverse {
    private static void printUsage() {
        System.out.println("Usage: java Reverse STRING");
    }
    private static String reverse(String str) {
        String ret = "";
        for (int i = 0; i < str.length(); ++i) {
            ret = str.charAt(i) + ret;
        }
        return ret;
    }
    public static void main(String [] argv) {
        if (argv.length != 1) {
            printUsage();
            System.exit(1);
        }
        System.out.println(reverse(argv[0]));
    }
}
