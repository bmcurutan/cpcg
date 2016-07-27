int fibonacci(int n);
int fibonacciRec(int f, int n1, int n2, int i, int n);
int fibonacciIter(int f, int n1, int n2, int i, int n);

   int fibonacci(int n) {
    if (n < 2)
      return n;
    else 
     //return fibonacciRec(n,0,1,2,n);
     return fibonacciIter(n,0,1,2,n);
  }
  
   int fibonacciRec(int f, int n1, int n2, int i, int n) {
    if (i <= n) {      
      return fibonacciRec(n1+n2,n2,n1+n2,i+1,n);
    }
    else {
      return f;
    }
  }
  
   int fibonacciIter(int f, int n1, int n2, int i, int n) {
    while (i <= n) {        
      f = n1 + n2;
      n1 = n2;
      n2 = f;
      i = i + 1;
    }
    return f;
  }
  
int main() {
    int i = 0;
	while(i<7) {
		printf("%d ", fibonacci(i));
		i++;
	}
	gets(); //to show window
	return -1;
}

