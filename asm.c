// how to get ARM64 assembly on mac:
// clang -isysroot $(xcrun --sdk iphoneos --show-sdk-path) -arch arm64 asm.c -o asm.o
// objdump -disassemble asm.o
#include <stdio.h>
int main( ) {
printf("Hello, world!\n");
}
// then parse it in Erlang!
