#import <Foundation/Foundation.h>
@interface HelloProg: NSObject
- (void) hello;
@end
@implementation HelloProg
- (void) hello {
    printf("Hello World!");
}
@end
