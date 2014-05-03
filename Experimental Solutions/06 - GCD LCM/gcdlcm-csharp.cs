using System;

class GCDLCM {
    public static int Gcdiv(int a, int b) {
        //return GcdRec(a,a,b);
      return GcdIter(a,a,b);
    }
    
    private static int GcdRec(int g,int a, int b) {
    if (b != 0) {
            return GcdRec(b,b,a % b);
        }
        else {
            return a;
        }
    }
    
    private static int GcdIter(int g,int a, int b) {
      while (b != 0) {
        g = b;
        b = a%b;
        a = g;
      }
      return a;
    }
    
    public static int Lcmul(int a, int b) {
        int y = a * b;
        int z = Gcdiv(a,b);
        return y / z;
 }

    static void Main() {
Console.WriteLine(Gcdiv(48,16));
Console.WriteLine(Lcmul(3,4));
}
}