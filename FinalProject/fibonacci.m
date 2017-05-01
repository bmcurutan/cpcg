#import <Foundation/Foundation.h>
@interface FibonacciProg:NSObject
- (long)fibonacci:(long)n;
- (long)fibonacciIter:(long)f n1:(long)n1 n2:(long)n2 i:(long)i n:(long)n;
@end
@implementation FibonacciProg
- (long) fibonacci:(long)n {
    /* The Fibonacci numbers are the numbers in the following integer sequence:
     * 0,1,1,2,3,5,8,13,... */
    // The first two numbers are 0 and 1, and each subsequent number is the sum of the previous two.
    if (n < 2) {
        return n;
    }
    else {
        return [self fibonacciItern:n 0:0 1:1 2:2 n];
    }
}
- (long) fibonacciIter:(long)f n1:(long)n1 n2:(long)n2 i:(long)i n:(long)n {
    while (i <= n) {
        f = n1 + n2;
        n1 = n2;
        n2 = f;
        i = i + 1;
    }
    return f;
}
@end
