using System;

class GCDLCM {
    public static int Gcdiv(int a, int b) {
        while (b != 0) {
            int c = a % b;
            a = b;
            b = c;
        }
        return a;
    }
    
    public static int Lcmul(int a, int b) {
        return (a*b)/Gcdiv(a,b);
    }
}