#import <Foundation/Foundation.h>

@interface Maximum : NSObject
- (int)maxi:(int)a b:(int)b;
@end


@implementation Maximum
- (int)maxi:(int)a b:(int)b {
    return MAX(a,b);
}
@end