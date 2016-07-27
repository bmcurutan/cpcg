public class Exponent {
  public int exponent(int a, int b) {
    //return exponentRec(1,a,b);
    return exponentIter(1,a,b);
  }
  
  private int exponentRec(int e, int a, int b) {
    if (b != 0) {
      return exponentRec(e*a,a,b - 1);
    }
    else {
      return e;
    }
  }
  
  private int exponentIter(int e, int a , int b) {
    while (b != 0) {
      e = e * a;
      b = b-1;
    }
    return e;
  }
}