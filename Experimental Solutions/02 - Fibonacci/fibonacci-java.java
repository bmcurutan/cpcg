public class Fibonacci {
  public int fibonacci(int n) {
    if (n < 2)
      return n;
    else 
     return fibonacciRec(n,0,1,2,n);
     //return fibonacciIter(n,0,1,2,n);
  }
  
  private int fibonacciRec(int f, int n1, int n2, int i, int n) {
    if (i <= n) {      
      return fibonacciRec(n1+n2,n2,n1+n2,i+1,n);
    }
    else {
      return f;
    }
  }
  
  private int fibonacciIter(int f, int n1, int n2, int i, int n) {
    while (i <= n) {        
      f = n1 + n2;
      n1 = n2;
      n2 = f;
      i = i + 1;
    }
    return f;
  }
  
  public static void main(String[] args) {
    for (int i = 0; i < 7; i++) {
      System.out.print(fibonacci(i) + " ");
    }
    System.out.println();
  }
}

/*public class Fibonacci {
    public Fibonacci() {
    }

    public int fibonacci(int n) {
        if (n <= 1) return n;
        else return fib(n-1) + fib(n-2);
    }
}*/