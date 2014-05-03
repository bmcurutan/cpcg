public class Factorial {
  public int factorial(int i) {
    //return factorialRec(1,i);
    return factorialIter(1,i);
  }
  
  private int factorialRec(int f, int i) {
    if (i > 0)
      return factorialRec(f*i,i-1);
    else
      return f;
  }
  
  private int factorialIter(int f, int i) {
    while (i > 0) {
      f = f*i;
      i = i-1;
    }
    return f;
  }
    
    public static void main(String[] args) {
      System.out.println(factorial(0) + " " + factorial(1) + " " + 
                         factorial(2) + " " + factorial(3));
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