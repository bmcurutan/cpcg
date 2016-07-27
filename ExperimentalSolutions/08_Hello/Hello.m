#import <Foundation/Foundation.h>

@interface Hello : NSObject
- (void)hello;
@end

@implementation Hello
- (void)hello {
    NSLog(@"Hello World!");
}
@end

/*int main(int argc, const char * argv[])
{
    @autoreleasepool {
        Hello *hel = [[Hello alloc] init];
        [hel hello];
    }
    return 0;
}*/
