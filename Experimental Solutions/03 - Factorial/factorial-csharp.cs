using System; //for Console.Write

class FactorialProg {
  public static int Factorial(int i) {
    return FactorialRec(1,i);
    //return FactorialIter(1,i);
  }
  
  private static int FactorialRec(int f, int i) {
    if (i > 0)
      return FactorialRec(f*i,i-1);
    else
      return f;
  }
  
  private static int FactorialIter(int f, int i) {
    while (i > 0) {
      f = f*i;
      i = i-1;
    }
    return f;
  }
    
    static void Main() {
    Console.WriteLine(Factorial(0) + " " + Factorial(1) + " " + 
                         Factorial(2) + " " + Factorial(3));
    }
}

/*public class Fibonacci {
    public Factorial() {
    }

    public int factorial(int n) {
        if (n < 1) return n;
        else return fac(n) * fac(n-1);
    }
}*/