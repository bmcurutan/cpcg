#import <Foundation/Foundation.h>

@interface Factorial : NSObject
- (int)factorial:(int)i;
@end

@implementation Factorial
- (int)factorial:(int)i {
    int f = 1;
    for (int j = 1; j <= i; j++) {
        f *= j;
    }
    return f;
}

