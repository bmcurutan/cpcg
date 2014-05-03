using System; //for Console.Write

class FibonacciProg {
  public static int Fibonacci(int n) {
    if (n < 2)
      return n;
    else 
     //return FibonacciRec(n,0,1,2,n);
     return FibonacciIter(n,0,1,2,n);
  }
  
  private static int FibonacciRec(int f, int n1, int n2, int i, int n) {
    if (i <= n) {      
      return FibonacciRec(n1+n2,n2,n1+n2,i+1,n);
    }
    else {
      return f;
    }
  }
  
  private static int FibonacciIter(int f, int n1, int n2, int i, int n) {
    while (i <= n) {        
      f = n1 + n2;
      n1 = n2;
      n2 = f;
      i = i + 1;
    }
    return f;
  }
    
    static void Main() {
        for (int i = 0; i < 7; i++) {
			Console.Write(Fibonacci(i) + " ");
        }
		Console.WriteLine();
    }
}