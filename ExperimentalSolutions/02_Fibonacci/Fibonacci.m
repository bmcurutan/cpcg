#import <Foundation/Foundation.h>

@interface Fibonacci : NSObject
- (int)fibonacci:(int)n;
- (int)fibonacciRec:(int)f n1:(int)n1 n2:(int)n2 i:(int)i n:(int)n;
- (int)fibonacciIter:(int)f n1:(int)n1 n2:(int)n2 i:(int)i n:(int)n;
@end

@implementation Fibonacci
- (int)fibonacci:(int)n {
/*    int n1 = 0;
    int n2 = 1;
    int i = 2;
    int f = n;
    
    while (i <= n) {
        f = n1 + n2;
        n1 = n2;
        n2 = f;
        i = i + 1;
    }
    return f;*/
 
    if (n < 2) {
        return n;
    }
    else {
        //return [self fibonacciRec:n n1:0 n2:1 i:2 n:n];
        return [self fibonacciIter:n n1:0 n2:1 i:2 n:n];
    }
}

- (int)fibonacciRec:(int)f n1:(int)n1 n2:(int)n2 i:(int)i n:(int)n {
    if (i <= n) {
        return [self fibonacciRec:n1+n2 n1:n2 n2:n1+n2 i:i+1 n:n];
    }
    else {
        return f;
    }
}

- (int)fibonacciIter:(int)f n1:(int)n1 n2:(int)n2 i:(int)i n:(int)n {
    while (i <= n) {
         f = n1+n2;
         n1 = n2;
         n2 = f;
         i = i+1;
    }
    return f;
}
@end

int main(int argc, const char * argv[])
{

    @autoreleasepool {
        Fibonacci *fib = [[Fibonacci alloc] init];
        for (int i = 0; i < 7; i++) {
            NSLog(@"%i",[fib fibonacci:i]);
        }
    }
    return 0;
}
