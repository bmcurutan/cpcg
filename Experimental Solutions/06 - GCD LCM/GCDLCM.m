#import <Foundation/Foundation.h>

@interface GCDLCM : NSObject
- (int)gcdiv:(int)a b:(int)b;
- (int)gcdivRec:(int)g a:(int)a b:(int)b;
- (int)gcdivIter:(int)g a:(int)a b:(int)b;
- (int)lcmul:(int)a b:(int)b;
@end

@implementation GCDLCM
- (int)gcdiv:(int)a b:(int)b {
//    return [self gcdivRec:a a:a b:b];
      return [self gcdivIter:a a:a b:b];
}
- (int)gcdivRec:(int)g a:(int)a b:(int)b {
    if (b != 0) {
        return [self gcdivRec:b a:b b:a % b];
    }
    else {
        return a;
    }
}

- (int)gcdivIter:(int)g a:(int)a b:(int)b {
    while (b != 0) {
        int g = b;
        b = a % b;
        a = g;
    }
    return a;
}

- (int)lcmul:(int)a b:(int)b {
    int y = a * b;
    int z = [self gcdiv:a b:b];
    return y / z;
}
@end

int main(int argc, const char * argv[])
{

    @autoreleasepool {
        
        GCDLCM *gcdlcm = [[GCDLCM alloc] init];
        NSLog(@"%i", [gcdlcm gcdiv:35 b:14]);
        NSLog(@"%i", [gcdlcm lcmul:35 b:14]);
    }
    return 0;
}