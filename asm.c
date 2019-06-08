// clang -isysroot $(xcrun --sdk iphoneos --show-sdk-path) -arch arm64 asm.c -o asm
#include <stdio.h>
int main( ) {
printf("Hello, world!\n");
return 0;
}
