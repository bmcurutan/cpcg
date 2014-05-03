long factorial(long a);

  long factorial(long a) {
    long f = 1;
    long i = 1;
    while (i<=a) {
      f *= i;
      i++;
    }
    return f;
  }
  
