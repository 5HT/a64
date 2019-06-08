// how to get ARM64 assembly on mac:
// clang -isysroot $(xcrun --sdk iphoneos --show-sdk-path) -arch arm64 asm.c -o asm
// objdump -disassemble asm
#include <stdio.h>
int main( ) {
int i = 1;
printf("Hello, world!\n");
if (i < 0) return i; else return 0;
}
// then parse it in Erlang!
