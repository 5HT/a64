sub sp, sp, #32
stp x29, x30, [sp, #16]
add x29, sp, #16
adrp    x8, #0
add x8, x8, #4008
mov w0, #0
stur    w0, [x29, #-4]
mov x0, x8
bl  [#24,0x100007f78]
ldur    w9, [x29, #-4]
mov x0, x9
ldp x29, x30, [sp, #16]
add sp, sp, #32
ret
