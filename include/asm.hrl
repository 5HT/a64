-ifndef(A64_HRL).
-define(A64_HRL, true).

-define(x(C), (C==x0 orelse C==x1 orelse C==x2 orelse C==x3 orelse C==x4 orelse C==x5 orelse C==x6 orelse C==x7 orelse
               C==x8 orelse C==x9 orelse C==x10 orelse C==x11 orelse C==x12 orelse C==x13 orelse C==x14 orelse C==x15 orelse
               C==x16 orelse C==x17 orelse C==x18 orelse C==x19 orelse C==x20 orelse C==x21 orelse C==x22 orelse C==x23 orelse
               C==x24 orelse C==x25 orelse C==x26 orelse C==x27 orelse C==x28 orelse C==x29 orelse C==x30 orelse C==sp)).

-define(w(C), (C==w0 orelse C==w1 orelse C==w2 orelse C==w3 orelse C==w4 orelse C==w5 orelse C==w6 orelse C==w7 orelse
               C==w8 orelse C==w9 orelse C==w10 orelse C==w11 orelse C==w12 orelse C==w13 orelse C==w14 orelse C==w15 orelse
               C==w16 orelse C==w17 orelse C==w18 orelse C==w19 orelse C==w20 orelse C==w21 orelse C==w22 orelse C==w23 orelse
               C==w24 orelse C==w25 orelse C==w26 orelse C==w27 orelse C==w28 orelse C==w29 orelse C==w30 orelse C==wsp)).

-define(imm(X),   is_integer(X)).
-define(imm3(X),  is_integer(X) andalso X < 8).
-define(imm6(X),  is_integer(X) andalso X < 64).
-define(imm9(X),  is_integer(X) andalso X < 512).
-define(imm10(X), is_integer(X) andalso X < 1024).
-define(imm12(X), is_integer(X) andalso X < 4096).
-define(imm16(X), is_integer(X) andalso X < 65536).
-define(imm21(X), is_integer(X) andalso X < 2097152).
-define(sh(X), (X==lsl orelse X == lsr orelse asr)).

-endif.
