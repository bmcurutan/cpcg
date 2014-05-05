#import <Foundation/Foundation.h>

@interface Fibonacci : NSObject
- (int)fibonacci:(int)n;
@end

@implementation Fibonacci
- (int)fibonacci:(int)n {
    int n1 = 0;
    int n2 = 1;
    int i = 2;
    int f = n;
    
    while (i <= n) {
        f = n1 + n2;
        n1 = n2;
        n2 = f;
        i = i + 1;
    }
    return f;
}