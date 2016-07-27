int gcdiv(int a, int b);
int gcdivRec(int g,int a, int b);
int gcdivIter(int g,int a, int b);

     int gcdiv(int a, int b) {
        return gcdivRec(a,a,b);
    //  return gcdivIter(a,a,b);
    }
    
     int gcdivRec(int g,int a, int b) {
    if (b != 0) {
            return gcdivRec(b,b,a % b);
        }
        else {
            return a;
        }
    }
    
     int gcdivIter(int g,int a, int b) {
      while (b != 0) {
        g = b;
        b = a%b;
        a = g;
      }
      return a;
    }
    
     int lcmul(int a, int b) {
        int y = a * b;
        int z = gcdiv(a,b);
        return y / z;
 }


int main() {
    printf("%d ", gcdiv(28,35));
    printf("%d ", lcmul(2,7));
	gets();
	return 0;
}
