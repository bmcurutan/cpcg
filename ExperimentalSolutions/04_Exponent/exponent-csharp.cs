using System; //for Console.Write

class ExponentProg {
  public static int Exponent(int a, int b) {
    return ExponentRec(1,a,b);
    //return ExponentIter(1,a,b);
  }
  
  private static int ExponentRec(int e, int a, int b) {
    if (b != 0) {
      return ExponentRec(e*a,a,b - 1);
    }
    else {
      return e;
    }
  }
  
  private static int ExponentIter(int e, int a , int b) {
    while (b != 0) {
      e = e * a;
      b = b-1;
    }
    return e;
  }

 static void Main() 
{   Console.WriteLine(Exponent(2,3));
}
}