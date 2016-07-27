int factorial(int i);
int factorialRec(int f, int i);
int factorialIter(int f, int i);

int factorial(int i) {
    //return factorialRec(1,i);
    return factorialIter(1,i);
  }
  
  int factorialRec(int f, int i) {
    if (i > 0)
      return factorialRec(f*i,i-1);
    else
      return f;
  }
  
  int factorialIter(int f, int i) {
    while (i > 0) {
      f = f*i;
      i = i-1;
    }
    return f;
  }
  
int main() {
	int i = 0;
	while(i<6) {
		printf("%d ", factorial(i));
		i++;
	}
	gets(); //to show window
	return -1;
}
