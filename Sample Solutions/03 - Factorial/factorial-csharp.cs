using System; 

class FactorialProg {
  public static int Factorial(int a) {
    int f = 1;
    for (int i = 1; i <= a; i++) {
      f *= i;
    }
    return f;
  }
  
}
