#import <Foundation/Foundation.h>

@interface Exponent : NSObject
- (int)exponent:(int)a b:(int)b;
- (int)exponentRec:(int)e a:(int)a b:(int)b;
- (int)exponentIter:(int)e a:(int)a b:(int)b;
@end


@implementation Exponent
- (int)exponent:(int)a b:(int)b {
    //return pow(a,b);
    //return [self exponentRec:1 a:a b:b];
    return [self exponentIter:1 a:a b:b];
}

- (int)exponentRec:(int)e a:(int)a b:(int)b {
    if (b != 0) {
        return [self exponentRec:e*a a:a b:b-1];
    }
    else {
        return e;
    }
}

  
- (int)exponentIter:(int)e a:(int)a b:(int)b {
    while (b != 0) {
        e = e*a;
        b = b-1;
    }
    return e;
}
@end

  int main(int argc, const char * argv[])
{

    @autoreleasepool {
        
        Exponent *exp = [[Exponent alloc] init];
        NSLog(@"%i", [exp exponent:2 b:5]);
    }
    return 0;
}