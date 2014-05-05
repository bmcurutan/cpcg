#import <Foundation/Foundation.h>

@interface Maximum : NSObject
- (int)maxi:(int)a b:(int)b;
@end


@implementation Maximum
- (int)maxi:(int)a b:(int)b {
    return MAX(a,b);
}
@end

/*int main(int argc, const char * argv[])
{

    @autoreleasepool {
        Maximum *max = [[Maximum alloc] init];
        NSLog(@"%i",[max maxi:-13 b:1]);
    }
    return 0;
}*/
