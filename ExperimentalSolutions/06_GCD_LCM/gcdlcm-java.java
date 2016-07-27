public class GCDLCM {
    public int gcdiv(int a, int b) {
        return gcdivRec(a,a,b);
    //  return gcdivIter(a,a,b);
    }
    
    private int gcdivRec(int g,int a, int b) {
    if (b != 0) {
            return gcdivRec(b,b,a % b);
        }
        else {
            return a;
        }
    }
    
    private int gcdivIter(int g,int a, int b) {
      while (b != 0) {
        g = b;
        b = a%b;
        a = g;
      }
      return a;
    }
    
    public int lcmul(int a, int b) {
        int y = a * b;
        int z = gcdiv(a,b);
        return y / z;
 }
}