#import <Foundation/Foundation.h>

@interface GCDLCM : NSObject
- (int)gcdiv:(int)a b:(int)b;
- (int)lcmul:(int)a b:(int)b;
@end

@implementation GCDLCM
- (int)gcdiv:(int)a b:(int)b {
    while (b != 0) {
        int c = a % b;
        a = b;
        b = c;
    }
    return a;
}
- (int)lcmul:(int)a b:(int)b {
    return (a*b) / [self gcdiv:a b:b];
}
@end