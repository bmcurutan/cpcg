 int exponent(int a, int b);  
int exponentIter(int e, int a , int b) ;
int exponentRec(int e, int a, int b) ;

 int exponent(int a, int b) {
    //return exponentRec(1,a,b);
    return exponentIter(1,a,b);
  }
  
   int exponentRec(int e, int a, int b) {
    if (b != 0) {
      return exponentRec(e*a,a,b - 1);
    }
    else {
      return e;
    }
  }
  
   int exponentIter(int e, int a , int b) {
    while (b != 0) {
      e = e * a;
      b = b-1;
    }
    return e;
  }

int main() {
    printf("%d ", exponent(2,3));
	gets();
	return 0;
}
