#import <Foundation/Foundation.h>
#include <ResourcePath.hpp>

std::string resource_path() {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  std::string rpath;
  NSBundle *bundle = [NSBundle mainBundle];

  if (bundle == nil) {
#ifdef DEBUG
    NSLog(@"bundle is nil... thus no resources path can be found.");
#endif
  } else {
    NSString *path = [bundle resourcePath];
    rpath = [path UTF8String] + std::string("/");
  }

  [pool drain];
  return rpath;
}
