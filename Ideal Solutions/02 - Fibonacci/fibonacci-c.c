long fibonacci(long n);

long fibonacci(long n) {
    long n1 = 0;
    long n2 = 1; 
    long i = 2;
    long f = n;

    while (i <= n) {        
        f = n1 + n2;
        n1 = n2;
        n2 = f;
        i = i + 1;
    }
    return f;
  }


