#import <Foundation/Foundation.h>

@interface Exponent : NSObject
- (int)exponent:(int)a b:(int)b;
@end


@implementation Exponent
- (int)exponent:(int)a b:(int)b {
    return pow(a,b);
}