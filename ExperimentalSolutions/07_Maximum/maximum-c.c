int maxi(int a, int b);

int maxi(int a, int b) {
    return ( a > b ) ? a : b;
    //return max(n, m);
}

int main() {
	int max1 = maxi(5,4);
	printf("%d ", max1);
	gets(); //to show window
	return -1;
}
