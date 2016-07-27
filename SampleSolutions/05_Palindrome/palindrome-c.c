#include <stdbool.h>
bool palindrome(char* a);
bool palindrome(char* a) {
    char b[256];
    strcpy(b,a);
	strrev(b);
	
	return strcmp(b,a) == 0; 
}
