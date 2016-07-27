public class Factorial {
    public static int factorial(int n) {
        int f = 1; // this  will be the result
        for (int i = 1; i <= n; i++) {
            f *= i;
        }
        return f;
    }
}
