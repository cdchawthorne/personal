class NonZeros {
    public static int [] nonZeros(int [] A) {
        int newArrayLength = 0;
        for (int i = 0; i < A.length; ++i) {
            if (A[i] != 0) {
                ++newArrayLength;
            }
        }
        int [] ret = new int [newArrayLength];
        int j = 0;
        for (int i = 0; i < A.length; ++i) {
            if (A[i] != 0) {
                ret[j] = A[i];
                ++j;
            }
        }
        return ret;
    }
    public static void printArray(int [] A) {
        System.out.print("[");
        for (int i = 0; i < A.length; ++i) {
            System.out.print(A[i]);
            if (i < A.length - 1) {
                System.out.print(",");
            }
        }
        System.out.print("]\n");
    }
    public static void main(String [] argv) {
        int [] A = {1,0,2,1};
        printArray(nonZeros(A));
    }
}
