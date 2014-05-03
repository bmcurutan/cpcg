#include <math.h> 

long exponent(long a, long b);  

 long exponent(long a, long b) {
    return pow(a,b);
  }
  
long main() {
    printf("%d ", exponent(2,3));
    gets();
	return 0;
}
