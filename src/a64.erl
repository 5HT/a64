-module(a64).
-copyright('ARM64 Assembler (c) SYNRC').
-author('Maxim Sokhatsky').
-include("asm.hrl").
-compile(export_all).

main([F])    -> {ok,I} = file:read_file(F), {C,O} = compile(code(I)), %io:format("~p~n",[code(I)]),
                file:write_file(base(F),O,[raw,write,binary,create]), halt(C);
main(_)      -> io:format("usage: a64 <file>\n"), halt(1).
base(X)      -> filename:basename(X,filename:extension(X)).
atom("#"++X) -> list_to_integer(X);
atom(X)      -> list_to_atom(X).

last(X,Y,A)  ->
  case lists:reverse(X) of
      "]"++Z -> [lists:reverse([atom(lists:reverse(Z))|Y])|A];
           _ -> {[atom(X)|Y],A} end.

code(Bin) ->
  [ lists:reverse(
    lists:foldl(
      fun([$[|X],A) -> last(X,[],A);
         (X,{Y,A}) -> last(X,Y,A);
         (X,A) -> [atom(X)|A] end,[],string:tokens(C," ,")))
   || C <- string:tokens(binary_to_list(Bin),"\n") ].

success(M,F,A) -> try erlang:apply(M,F,A) catch _:_ -> <<>> end.
success_(M,F,A) -> erlang:apply(M,F,A).

compile(Code) ->
   {0,iolist_to_binary([ success(?MODULE,hd(Instr),tl(Instr)) || Instr <- Code])}.

% register, extend and option encoding

reg(sp)    -> <<31:5>>;
reg(wsp)   -> <<31:5>>;
reg(X)     -> <<(list_to_integer(tl(atom_to_list(X)))):5>>.

shift(lsl) -> <<0:2>>;
shift(lsr) -> <<1:2>>;
shift(asr) -> <<2:2>>;
shift(ror) -> <<3:2>>.

extend(uxtb) -> <<0:3>>;
extend(uxth) -> <<1:3>>;
extend(uxtw) -> <<2:3>>;
extend(uxtx) -> <<3:3>>;
extend(sxtb) -> <<4:3>>;
extend(sxth) -> <<5:3>>;
extend(sxtw) -> <<6:3>>;
extend(sxtx) -> <<7:3>>.

sys(s1e1r)   -> {<<0:3>>,<<0:1>>,<<0:3>>};
sys(s1e1w)   -> {<<0:3>>,<<0:1>>,<<1:3>>};
sys(s1e0r)   -> {<<0:3>>,<<0:1>>,<<2:3>>};
sys(s1e0w)   -> {<<0:3>>,<<0:1>>,<<3:3>>};
sys(s1e2r)   -> {<<4:3>>,<<0:1>>,<<0:3>>};
sys(s1e2w)   -> {<<4:3>>,<<0:1>>,<<1:3>>};
sys(s12e1r)  -> {<<4:3>>,<<0:1>>,<<4:3>>};
sys(s12e1w)  -> {<<4:3>>,<<0:1>>,<<5:3>>};
sys(s12e0r)  -> {<<4:3>>,<<0:1>>,<<6:3>>};
sys(s12e0w)  -> {<<4:3>>,<<0:1>>,<<7:3>>};
sys(s1e3r)   -> {<<6:3>>,<<0:1>>,<<0:3>>};
sys(s1e3w)   -> {<<6:3>>,<<0:1>>,<<1:3>>};
sys(s1e1rp)  -> {<<0:3>>,<<1:1>>,<<0:3>>};
sys(s1e1wp)  -> {<<0:3>>,<<1:1>>,<<1:3>>}.

% C6.2.1 ADC

adc(R1,R2,R3) when ?x(R1), ?x(R2), ?x(R3) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1),
   <<1:1,0:2,13:4,0:4,Rm/bitstring,0:6,Rn/bitstring,Rd/bitstring>>;

adc(R1,R2,R3) when ?w(R1), ?w(R2), ?w(R3) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1),
   <<0:1,0:2,13:4,0:4,Rm/bitstring,0:6,Rn/bitstring,Rd/bitstring>>.

% C6.2.2 ADCS

adcs(R1,R2,R3) when ?x(R1), ?x(R2), ?x(R3) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1),
   <<1:1,1:2,13:4,0:4,Rm/bitstring,0:6,Rn/bitstring,Rd/bitstring>>;

adcs(R1,R2,R3) when ?w(R1), ?w(R2), ?w(R3) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1),
   <<0:1,1:2,13:4,0:4,Rm/bitstring,0:6,Rn/bitstring,Rd/bitstring>>.

% C6.2.3 ADD (extended register)

add(R1,R2,R3) when ?x(R1), ?x(R2), ?x(R3) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<0:3>>,
   <<1:1,0:1,0:1,11:5,1:3,Rm/bitstring,3:3,I/bitstring,Rn/bitstring,Rd/bitstring>>;

add(R1,R2,[R3,Im]) when ?x(R1), ?x(R2), ?x(R3), ?imm3(Im) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:3>>,
   <<1:1,0:1,0:1,11:5,1:3,Rm/bitstring,3:3,I/bitstring,Rn/bitstring,Rd/bitstring>>;

add(R1,R2,R3) when ?x(R1), ?x(R2), ?w(R3) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<0:3>>,
   <<1:1,0:1,0:1,11:5,1:3,Rm/bitstring,3:3,I/bitstring,Rn/bitstring,Rd/bitstring>>;

add(R1,R2,[R3,Im]) when ?x(R1), ?x(R2), ?w(R3), ?imm3(Im)  ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:3>>,
   <<1:1,0:1,0:1,11:5,1:3,Rm/bitstring,3:3,I/bitstring,Rn/bitstring,Rd/bitstring>>;

add(R1,R2,R3) when ?w(R1), ?w(R2), ?w(R3) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<0:3>>,
   <<0:1,0:1,0:1,11:5,1:3,Rm/bitstring,6:3,I/bitstring,Rn/bitstring,Rd/bitstring>>;

add(R1,R2,[R3,Im]) when ?w(R1), ?w(R2), ?w(R3), ?imm3(Im) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:3>>,
   <<0:1,0:1,0:1,11:5,1:3,Rm/bitstring,6:3,I/bitstring,Rn/bitstring,Rd/bitstring>>;

% C6.2.4 ADD (immediate) % C6.2.173 MOV (to/from SP)

add(R1,R2,Im) when ?x(R1), ?x(R2), ?imm12(Im) ->
   Dst = reg(R1), Src = reg(R2), I = <<Im:12>>,
   <<1:1,0:1,0:1,34:6,0:1,I/bitstring,Src/bitstring,Dst/bitstring>>;

add(R1,R2,Im) when ?w(R1), ?w(R2), ?imm12(Im) ->
   Dst = reg(R1), Src = reg(R2), I = <<Im:12>>,
   <<0:1,0:1,0:1,34:6,0:1,I/bitstring,Src/bitstring,Dst/bitstring>>.

% C6.2.5 ADD (shifted register)

add(R1,R2,R3,Sh,Im) when ?x(R1), ?x(R2), ?x(R3), ?imm6(Im), ?sh3(Sh) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:6>>, S = shift(Sh),
   <<1:1,0:1,0:1,11:5,S/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

add(R1,R2,R3,Sh,Im) when ?w(R1), ?w(R2), ?w(R3), ?imm6(Im), ?sh3(Sh) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:6>>, S = shift(Sh),
   <<0:1,0:1,0:1,11:5,S/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>.

% C6.2.6 ADDS (extended register) % C6.2.53 CMN (extended register)

adds(R1,R2,R3,Ex,Im) when ?x(R1), ?x(R2), ?x(R3), ?imm3(Im), ?extend(Ex) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:3>>, S = extend(Ex),
   <<1:1,1:2,11:5,1:3,Rm/bitstring,S/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

adds(R1,R2,R3,Ex,Im) when ?x(R1), ?x(R2), ?w(R3), ?imm3(Im), ?extend(Ex) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:3>>, S = extend(Ex),
   <<1:1,1:2,11:5,1:3,Rm/bitstring,S/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

adds(R1,R2,R3,Ex,Im) when ?w(R1), ?w(R2), ?w(R3), ?imm3(Im), ?extend(Ex) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:3>>, S = extend(Ex),
   <<0:1,1:2,11:5,1:3,Rm/bitstring,S/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

% C6.2.8 ADDS (shifted register) % C6.2.55 CMN (shifted register)

adds(R1,R2,R3,Sh,Im) when ?x(R1), ?x(R2), ?x(R3), ?imm6(Im), ?sh3(Sh) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:6>>, S = shift(Sh),
   <<1:1,1:2,11:5,S/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

adds(R1,R2,R3,Sh,Im) when ?w(R1), ?w(R2), ?w(R3), ?imm6(Im), ?sh3(Sh) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:6>>, S = shift(Sh),
   <<0:1,1:2,11:5,S/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>.

% C6.2.7 ADDS (immediate) % C6.2.54 CMN (immediate)

adds(R1,R2,Im) when ?x(R1), ?x(R2), ?imm12(Im) ->
   Dst = reg(R1), Src = reg(R2), I = <<Im:12>>, Sh = <<1:1>>,
   <<1:1,1:2,34:6,Sh/bitstring,I/bitstring,Src/bitstring,Dst/bitstring>>;

adds(R1,R2,Im) when ?w(R1), ?w(R2), ?imm12(Im) ->
   Rd = reg(R1), Rn = reg(R2), I = <<Im:12>>, Sh = <<1:1>>,
   <<0:1,1:2,34:6,Sh/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>.

% C6.2.9 ADR

adr(R1,Im) when ?x(R1), ?imm21(Im) ->
   Dst = reg(R1), I = <<(Im bsr 2):19>>, J = <<Im:2>>,
   <<0:1,J:2/bitstring,16:5,I:19/bitstring,Dst:5/bitstring>>.

% C6.2.10 ADRP

adrp(R1,Im) when ?x(R1), ?imm21(Im) ->
   Dst = reg(R1), I = <<(Im bsr 2):19>>, J = <<Im:2>>,
   <<1:1,J:2/bitstring,16:5,I:19/bitstring,Dst:5/bitstring>>.

% C6.2.11 AND (immediate)

'and'(R1,R2,Im) when ?x(R1), ?x(R2), ?imm13(Im) ->
   Rd = reg(R1), Rn = reg(R2), Immr = <<Im:6>>, Imms = <<(Im bsr 6):6>>, H = <<(Im bsr 12):1>>, Sh = <<0:2>>,
   <<1:1,0:2,9:4,Sh/bitstring,H/bitstring,Immr/bitstring,Imms/bitstring,Rn/bitstring,Rd/bitstring>>;

'and'(R1,R2,Im) when ?w(R1), ?w(R2), ?imm12(Im) ->
   Rd = reg(R1), Rn = reg(R2), Immr = <<Im:6>>, Imms = <<(Im bsr 6):6>>, Sh = <<0:2>>,
   <<0:1,0:2,9:4,Sh/bitstring,0:1,Immr/bitstring,Imms/bitstring,Rn/bitstring,Rd/bitstring>>;

% C6.2.12 AND (shifted register)

'and'(R1,R2,R3) when ?x(R1), ?x(R2), ?x(R3) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2), I = <<0:6>>, Sh = <<0:2>>,
   <<1:1,0:2,10:5,Sh/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

'and'(R1,R2,R3) when ?w(R1), ?w(R2), ?w(R3) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2), I = <<0:6>>, Sh = <<0:2>>,
   <<0:1,0:2,10:5,Sh/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>.

'and'(R1,R2,R3,Sh,Im) when ?x(R1), ?x(R2), ?x(R3), ?sh4(Sh), ?imm6(Im) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2), I = <<Im:6>>, S = shift(Sh),
   <<1:1,0:2,10:5,S/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

'and'(R1,R2,R3,Sh,Im) when ?w(R1), ?w(R2), ?w(R3), ?sh4(Sh), ?imm6(Im) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2), I = <<Im:6>>, S = shift(Sh),
   <<0:1,0:2,10:5,S/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>.

% C6.2.13 ANDS (immediate) % C6.2.307 TST (immediate)

ands(R1,R2,Im) when ?x(R1), ?x(R2), ?imm13(Im) ->
   Rd = reg(R1), Rn = reg(R2), Immr = <<Im:6>>, Imms = <<(Im bsr 6):6>>, H = <<(Im bsr 12):1>>, Sh = <<0:2>>,
   <<1:1,3:2,9:4,Sh/bitstring,H/bitstring,Immr/bitstring,Imms/bitstring,Rn/bitstring,Rd/bitstring>>;

ands(R1,R2,Im) when ?w(R1), ?w(R2), ?imm12(Im) ->
   Rd = reg(R1), Rn = reg(R2), Immr = <<Im:6>>, Imms = <<(Im bsr 6):6>>, Sh = <<0:2>>,
   <<0:1,3:2,9:4,Sh/bitstring,0:1,Immr/bitstring,Imms/bitstring,Rn/bitstring,Rd/bitstring>>;

% C6.2.14 ANDS (shifted register) % C6.2.308 TST (shifted register)

ands(R1,R2,R3) when ?x(R1), ?x(R2), ?x(R3) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2), I = <<0:6>>, Sh = <<0:2>>,
   <<1:1,3:2,10:5,Sh/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

ands(R1,R2,R3) when ?w(R1), ?w(R2), ?w(R3) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2), I = <<0:6>>, Sh = <<0:2>>,
   <<0:1,3:2,10:5,Sh/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>.

ands(R1,R2,R3,Sh,Im) when ?x(R1), ?x(R2), ?sh4(Sh), ?imm6(Im) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2), I = <<Im:6>>, S = shift(Sh),
   <<1:1,3:2,10:5,S/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

ands(R1,R2,R3,Sh,Im) when ?w(R1), ?w(R2), ?sh4(Sh), ?imm6(Im) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2), I = <<Im:6>>, S = shift(Sh),
   <<0:1,3:2,10:5,S/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>.

% C6.2.15 ASR (register) % C6.2.17 ASRV

asr(R1,R2,R3) when ?x(R1), ?x(R2), ?x(R3) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2),
   <<1:1,0:2,13:4,6:4,Rm/bitstring,2:4,2:2,Rn/bitstring,Rd/bitstring>>;

asr(R1,R2,R3) when ?w(R1), ?w(R2), ?w(R3) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2),
   <<0:1,0:2,13:4,6:4,Rm/bitstring,2:4,2:2,Rn/bitstring,Rd/bitstring>>.

% C6.2.16 ASR (immediate) % C6.2.219 SBFIZ % C6.2.220 SBFM % C6.2.221 SBFX % C6.2.298 SXTB % C6.2.299 SXTH % C6.2.300 SXTW

% C6.2.18 AT

at(Op,X) when ?op(Op), ?x(X) ->
   R = reg(X), {Op1,M,Op2} = sys(Op),
   <<13:4,5:4,1:5,Op1/bitstring,7:4,4:3,M/bitstring,Op2/bitstring,R/bitstring>>.

% C6.2.19 AUTDA, AUTDZA

autdza(R1) when ?x(R1) ->
   <<>>.

autda(R1,R2) when ?x(R1), ?x(R2) ->
   <<>>.

% C6.2.20 AUTDB, AUTDZB

% C6.2.21 AUTIA, AUTIA1716, AUTIASP, AUTIAZ, AUTIZA

% C6.2.22 AUTIB, AUTIB1716, AUTIBSP, AUTIBZ, AUTIZB

% C6.2.23 B.cond

% C6.2.24 B

% C6.2.25 BFC % C6.2.26 BFI % C6.2.27 BFM % C6.2.28 BFXIL

% C6.2.29 BIC (shifted register)

% C6.2.30 BICS (shifted register)

% C6.2.31 BL

% C6.2.32 BLR

% C6.2.33 BLRAA, BLRAAZ, BLRAB, BLRABZ

% C6.2.34 BR

% C6.2.35 BRAA, BRAAZ, BRAB, BRABZ

% C6.2.36 BRK

% C6.2.37 CASB, CASAB, CASALB, CASLB

% C6.2.38 CASH, CASAH, CASALH, CASLH

% C6.2.39 CASP, CASPA, CASPAL, CASPL

% C6.2.40 CAS, CASA, CASAL, CASL

% C6.2.41 CBNZ

% C6.2.42 CBZ

% C6.2.43 CCMN (immediate)

% C6.2.44 CCMN (register)

% C6.2.45 CCMP (immediate)

% C6.2.46 CCMP (register)

% C6.2.47 CFINV 

% C6.2.48 CINC % C6.2.66 CSINC % C6.2.64 CSET

% C6.2.49 CINV % C6.2.65 CSETM % C6.2.67 CSINV

% C6.2.50 CLREX

% C6.2.51 CLS

% C6.2.52 CLZ

% C6.2.56 CMP (extended register) % C6.2.291 SUBS (extended register)

% C6.2.57 CMP (immediate) % C6.2.292 SUBS (immediate)

% C6.2.58 CMP (shifted register) % C6.2.293 SUBS (shifted register) % C6.2.188 NEGS

% C6.2.59 CNEG % C6.2.68 CSNEG

% C6.2.60 CRC32B, CRC32H, CRC32W, CRC32X

% C6.2.61 CRC32CB, CRC32CH, CRC32CW, CRC32CX

% C6.2.62 CSDB

% C6.2.63 CSEL

% C6.2.69 DC

% C6.2.70 DCPS1

% C6.2.71 DCPS2

% C6.2.72 DCPS3

% C6.2.73 DMB

% C6.2.74 DRPS

% C6.2.75 DSB

% C6.2.76 EON (shifted register)

% C6.2.77 EOR (immediate)

% C6.2.78 EOR (shifted register)

% C6.2.79 ERET

% C6.2.80 ERETAA, ERETAB

% C6.2.81 ESB

% C6.2.82 EXTR % C6.2.214 ROR (immediate)

% C6.2.83 HINT

% C6.2.84 HLT

% C6.2.85 HVC

% C6.2.86 IC

% C6.2.87 ISB

% C6.2.88 LDADDB, LDADDAB, LDADDALB, LDADDLB % C6.2.233 STADDB, STADDLB

% C6.2.89 LDADDH, LDADDAH, LDADDALH, LDADDLH % C6.2.234 STADDH, STADDLH

% C6.2.90 LDADD, LDADDA, LDADDAL, LDADDL % C6.2.235 STADD, STADDL

% C6.2.91 LDAPR

% C6.2.92 LDAPRB

% C6.2.93 LDAPRH

% C6.2.94 LDAPUR

% C6.2.95 LDAPURB

% C6.2.96 LDAPURH

% C6.2.97 LDAPURSB

% C6.2.98 LDAPURSH

% C6.2.99 LDAPURSW

% C6.2.100 LDAR

% C6.2.101 LDARB

% C6.2.102 LDARH

% C6.2.103 LDAXP

% C6.2.104 LDAXR

% C6.2.105 LDAXRB

% C6.2.106 LDAXRH

% C6.2.107 LDCLRB, LDCLRAB, LDCLRALB, LDCLRLB % C6.2.236 STCLRB, STCLRLB

% C6.2.108 LDCLRH, LDCLRAH, LDCLRALH, LDCLRLH % C6.2.237 STCLRH, STCLRLH

% C6.2.109 LDCLR, LDCLRA, LDCLRAL, LDCLRL % C6.2.238 STCLR, STCLRL

% C6.2.110 LDEORB, LDEORAB, LDEORALB, LDEORLB % C6.2.239 STEORB, STEORLB

% C6.2.111 LDEORH, LDEORAH, LDEORALH, LDEORLH % C6.2.240 STEORH, STEORLH

% C6.2.112 LDEOR, LDEORA, LDEORAL, LDEORL % C6.2.241 STEOR, STEORL

% C6.2.113 LDLARB

% C6.2.114 LDLARH

% C6.2.115 LDLAR

% C6.2.116 LDNP

% C6.2.117 LDP

% C6.2.118 LDPSW

% C6.2.119 LDR (immediate)

% C6.2.120 LDR (literal)

% C6.2.121 LDR (register)

% C6.2.122 LDRAA, LDRAB

% C6.2.123 LDRB (immediate)

% C6.2.124 LDRB (register)

% C6.2.125 LDRH (immediate)

% C6.2.126 LDRH (register)

% C6.2.127 LDRSB (immediate)

% C6.2.128 LDRSB (register)

% C6.2.129 LDRSH (immediate)

% C6.2.130 LDRSH (register)

% C6.2.131 LDRSW (immediate)

% C6.2.132 LDRSW (literal)

% C6.2.133 LDRSW (register)

% C6.2.134 LDSETB, LDSETAB, LDSETALB, LDSETLB % C6.2.263 STSETB, STSETLB

% C6.2.135 LDSETH, LDSETAH, LDSETALH, LDSETLH % C6.2.264 STSETH, STSETLH

% C6.2.136 LDSET, LDSETA, LDSETAL, LDSETL % C6.2.265 STSET, STSETL

% C6.2.137 LDSMAXB, LDSMAXAB, LDSMAXALB, LDSMAXLB % C6.2.266 STSMAXB, STSMAXLB

% C6.2.138 LDSMAXH, LDSMAXAH, LDSMAXALH, LDSMAXLH % C6.2.267 STSMAXH, STSMAXLH

% C6.2.139 LDSMAX, LDSMAXA, LDSMAXAL, LDSMAXL % C6.2.268 STSMAX, STSMAXL

% C6.2.140 LDSMINB, LDSMINAB, LDSMINALB, LDSMINLB % C6.2.269 STSMINB, STSMINLB

% C6.2.141 LDSMINH, LDSMINAH, LDSMINALH, LDSMINLH % C6.2.270 STSMINH, STSMINLH

% C6.2.142 LDSMIN, LDSMINA, LDSMINAL, LDSMINL % C6.2.271 STSMIN, STSMINL

% C6.2.143 LDTR

% C6.2.144 LDTRB

% C6.2.145 LDTRH

% C6.2.146 LDTRSB

% C6.2.147 LDTRSH

% C6.2.148 LDTRSW

% C6.2.149 LDUMAXB, LDUMAXAB, LDUMAXALB, LDUMAXLB % C6.2.275 STUMAXB, STUMAXLB

% C6.2.150 LDUMAXH, LDUMAXAH, LDUMAXALH, LDUMAXLH % C6.2.276 STUMAXH, STUMAXLH

% C6.2.151 LDUMAX, LDUMAXA, LDUMAXAL, LDUMAXL % C6.2.277 STUMAX, STUMAXL

% C6.2.152 LDUMINB, LDUMINAB, LDUMINALB, LDUMINLB % C6.2.278 STUMINB, STUMINLB

% C6.2.153 LDUMINH, LDUMINAH, LDUMINALH, LDUMINLH % C6.2.279 STUMINH, STUMINLH

% C6.2.154 LDUMIN, LDUMINA, LDUMINAL, LDUMINL % C6.2.280 STUMIN, STUMINL

% C6.2.155 LDUR

% C6.2.156 LDURB

% C6.2.157 LDURH

% C6.2.158 LDURSB

% C6.2.159 LDURSH

% C6.2.160 LDURSW

% C6.2.161 LDXP

% C6.2.162 LDXR

% C6.2.163 LDXRB

% C6.2.164 LDXRH

% C6.2.165 LSL (register) % C6.2.167 LSLV

% C6.2.166 LSL (immediate) % C6.2.309 UBFIZ % C6.2.310 UBFM % C6.2.311 UBFX % C6.2.319 UXTB % C6.2.320 UXTH

% C6.2.168 LSR (register) % C6.2.170 LSRV

% C6.2.171 MADD % C6.2.185 MUL

% C6.2.172 MNEG % C6.2.184 MSUB

% C6.2.174 MOV (inverted wide immediate) % C6.2.179 MOVN

movn(R1,Im) when ?x(R1), ?imm16(Im) ->
   R = reg(R1), I = <<Im:16>>,
   <<1:1,0:2,37:6,0:2,I/bitstring,R/bitstring>>;

movn(R1,Im) when ?w(R1), ?imm16(Im) ->
   R = reg(R1), I = <<Im:16>>,
   <<0:1,0:2,37:6,0:2,I/bitstring,R/bitstring>>.

% C6.2.175 MOV (wide immediate) % C6.2.180 MOVZ

movz(R1,Im) when ?x(R1), ?imm16(Im) ->
   R = reg(R1), I = <<Im:16>>,
   <<1:1,2:2,37:6,0:2,I/bitstring,R/bitstring>>;

movz(R1,Im) when ?w(R1), ?imm16(Im) ->
   R = reg(R1), I = <<Im:16>>,
   <<0:1,2:2,37:6,0:2,I/bitstring,R/bitstring>>.

% C6.2.176 MOV (bitmask immediate) % C6.2.193 ORR (immediate)

orr(R1,Im) when ?x(R1), ?imm16(Im) ->
   R = reg(R1), Rm = <<Im:16>>,
   <<1:1,1:2,10:5,0:3,Rm/bitstring,R/bitstring>>;

orr(R1,Im) when ?w(R1), ?imm16(Im) ->
   R = reg(R1), Rm = <<Im:16>>,
   <<0:1,1:2,10:5,0:3,Rm/bitstring,R/bitstring>>.

% C6.2.177 MOV (register)

% C6.2.178 MOVK

% C6.2.181 MRS

% C6.2.182 MSR (immediate)

% C6.2.183 MSR (register)

% C6.2.186 MVN

% C6.2.189 NGC % C6.2.217 SBC

% C6.2.190 NGCS % C6.2.218 SBCS

% C6.2.191 NOP

% C6.2.192 ORN (shifted register)

% C6.2.195 PACDA, PACDZA

% C6.2.196 PACDB, PACDZB

% C6.2.197 PACGA

% C6.2.198 PACIA, PACIA1716, PACIASP, PACIAZ, PACIZA

% C6.2.199 PACIB, PACIB1716, PACIBSP, PACIBZ, PACIZB

% C6.2.200 PRFM (immediate)

% C6.2.201 PRFM (literal)

% C6.2.202 PRFM (register)

% C6.2.203 PRFUM

% C6.2.204 PSB CSYNC

% C6.2.205 PSSBB

% C6.2.206 RBIT

% C6.2.207 RET

% C6.2.208 RETAA, RETAB

% C6.2.209 REV % C6.2.212 REV64

% C6.2.210 REV16

% C6.2.211 REV32

% C6.2.213 RMIF

% C6.2.215 ROR (register) % C6.2.216 RORV

% C6.2.222 SDIV

% C6.2.223 SETF8, SETF16

% C6.2.224 SEV

% C6.2.225 SEVL

% C6.2.226 SMADDL % C6.2.231 SMULL

% C6.2.227 SMC

% C6.2.228 SMNEGL % C6.2.229 SMSUBL

% C6.2.230 SMULH

% C6.2.232 SSBB

% C6.2.242 STLLRB

% C6.2.243 STLLRH

% C6.2.244 STLLR

% C6.2.245 STLR

% C6.2.246 STLRB

% C6.2.247 STLRH

% C6.2.248 STLUR

% C6.2.249 STLURB

% C6.2.250 STLURH

% C6.2.251 STLXP

% C6.2.252 STLXR

% C6.2.253 STLXRB

% C6.2.254 STLXRH

% C6.2.255 STNP

% C6.2.256 STP

% Post-index

stp(R1,R2,[R3],Im) when ?w(R1), ?w(R2), ?x(R3), ?imm9(Im) ->
   Dst = reg(R1), Src = reg(R2), Rn = reg(R3), I = <<(Im div 4):7>>,
   <<1:2,5:3,0:1,1:3,0:1,I/bitstring,Src/bitstring,Rn/bitstring,Dst/bitstring>>;

stp(R1,R2,[R3],Im) when ?x(R1), ?x(R2), ?x(R3), ?imm10(Im) ->
   Dst = reg(R1), Src = reg(R2), Rn = reg(R3), I = <<(Im div 8):7>>,
   <<2:2,5:3,0:1,1:3,0:1,I/bitstring,Src/bitstring,Rn/bitstring,Dst/bitstring>>.

% Signed offset

stp(R1,R2,[R3,Im]) when ?w(R1), ?w(R2), ?x(R3), ?imm9(Im) ->
   Dst = reg(R1), Src = reg(R2), Rn = reg(R3), I = <<(Im div 4):7>>,
   <<1:2,5:3,0:1,2:3,0:1,I/bitstring,Src/bitstring,Rn/bitstring,Dst/bitstring>>;

stp(R1,R2,[R3,Im]) when ?x(R1), ?x(R2), ?x(R3), ?imm10(Im) ->
   Dst = reg(R1), Src = reg(R2), Rn = reg(R3), I = <<(Im div 8):7>>,
   <<2:2,5:3,0:1,2:3,0:1,I/bitstring,Src/bitstring,Rn/bitstring,Dst/bitstring>>;

% Pre-index

stp(R1,R2,[R3,Im,$!]) when ?w(R1), ?w(R2), ?x(R3), ?imm9(Im) ->
   Dst = reg(R1), Src = reg(R2), Rn = reg(R3), I = <<(Im div 4):7>>,
   <<1:2,5:3,0:1,3:3,0:1,I/bitstring,Src/bitstring,Rn/bitstring,Dst/bitstring>>;

stp(R1,R2,[R3,Im,$!]) when ?x(R1), ?x(R2), ?x(R3), ?imm10(Im) ->
   Dst = reg(R1), Src = reg(R2), Rn = reg(R3), I = <<(Im div 8):7>>,
   <<2:2,5:3,0:1,3:3,0:1,I/bitstring,Src/bitstring,Rn/bitstring,Dst/bitstring>>.

% C6.2.257 STR (immediate)

% C6.2.258 STR (register)

% C6.2.259 STRB (immediate)

% C6.2.260 STRB (register)

% C6.2.261 STRH (immediate)

% C6.2.262 STRH (register)

% C6.2.272 STTR

% C6.2.273 STTRB

% C6.2.274 STTRH

% C6.2.281 STUR

stur(R1,[R2,Im]) when ?x(R1), ?x(R2), ?imm9(Im) ->
   Rt = reg(R1), Rn = reg(R2), I = <<Im:9>>,
   <<3:2,7:3,0:6,I/bitstring,0:2,Rn/bitstring,Rt/bitstring>>;

stur(R1,[R2,Im]) when ?w(R1), ?x(R2), ?imm9(Im) ->
   Rt = reg(R1), Rn = reg(R2), I = <<Im:9>>,
   <<2:2,7:3,0:6,I/bitstring,0:2,Rn/bitstring,Rt/bitstring>>.

% C6.2.282 STURB

% C6.2.283 STURH

% C6.2.284 STXP

% C6.2.285 STXR

% C6.2.286 STXRB

% C6.2.287 STXRH

% C6.2.288 SUB (extended register)

% C6.2.289 SUB (immediate)

sub(R1,R2,Im) when ?x(R1), ?x(R2), ?imm12(Im) ->
   Dst = reg(R1), Src = reg(R2), I = <<Im:12>>,
   <<1:1,1:1,0:1,34:6,0:1,I/bitstring,Src/bitstring,Dst/bitstring>>;

sub(R1,R2,Im) when ?w(R1), ?w(R2), ?imm12(Im) ->
   Dst = reg(R1), Src = reg(R2), I = <<Im:12>>,
   <<0:1,1:1,0:1,34:6,0:1,I/bitstring,Src/bitstring,Dst/bitstring>>.

% C6.2.290 SUB (shifted register) % C6.2.187 NEG (shifted register)

% C6.2.294 SVC

% C6.2.295 SWPB, SWPAB, SWPALB, SWPLB

% C6.2.296 SWPH, SWPAH, SWPALH, SWPLH

% C6.2.297 SWP, SWPA, SWPAL, SWPL

% C6.2.301 SYS

% C6.2.302 SYSL

% C6.2.303 TBNZ

% C6.2.304 TBZ

% C6.2.305 TLBI

% C6.2.306 TSB CSYNC

% C6.2.312 UDF

% C6.2.313 UDIV

% C6.2.314 UMADDL % C6.2.318 UMULL

% C6.2.315 UMNEGL % C6.2.316 UMSUBL

% C6.2.317 UMULH

% C6.2.321 WFE

% C6.2.322 WFI

% C6.2.323 XPACD, XPACI, XPACLRI

% C6.2.324 YIELD