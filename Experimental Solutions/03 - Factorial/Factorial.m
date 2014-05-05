#import <Foundation/Foundation.h>

@interface Factorial : NSObject
- (int)factorial:(int)i;
- (int)factorialRec:(int)f i:(int)i;
- (int)factorialIter:(int)f i:(int)i;
@end

@implementation Factorial
- (int)factorial:(int)i {
/*    int f = 1;
    for (int j = 1; j <= i; j++) {
        f *= j;
    }
    return f;*/
   // return [self factorialRec:1 i:i];
    return [self factorialIter:1 i:i];
}

- (int)factorialRec:(int)f i:(int)i {
    if (i > 0) {
        return [self factorialRec:f*i i:i-1];
    }
    else {
        return f;
    }
}

- (int)factorialIter:(int)f i:(int)i {
    while (i > 0) {
        f = f*i;
        i = i-1;
    }
    return f;
}
@end

int main(int argc, const char * argv[])
{
    @autoreleasepool {
        Factorial *fac = [[Factorial alloc] init];
        NSLog(@"%i %i %i %i %i",[fac factorial:0],[fac factorial:1],[fac factorial:2],[fac factorial:3],[fac factorial:4]);
    }
}

