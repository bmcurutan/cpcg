public class GCDLCM {
    public int gcdiv(int a, int b) {
        while (b != 0) {
            int c = a % b;
            a = b;
            b = c;
        }
        return a;
    }
    
    public int lcmul(int a, int b) {
        return (a*b) / gcdiv(a,b);
    }
}