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

% C6.2.4 ADD (immediate)

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

% C6.2.6 ADDS (extended register)

adds(R1,R2,R3,Ex,Im) when ?x(R1), ?x(R2), ?x(R3), ?imm3(Im), ?extend(Ex) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:3>>, S = extend(Ex),
   <<1:1,1:2,11:5,1:3,Rm/bitstring,S/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

adds(R1,R2,R3,Ex,Im) when ?x(R1), ?x(R2), ?w(R3), ?imm3(Im), ?extend(Ex) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:3>>, S = extend(Ex),
   <<1:1,1:2,11:5,1:3,Rm/bitstring,S/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

adds(R1,R2,R3,Ex,Im) when ?w(R1), ?w(R2), ?w(R3), ?imm3(Im), ?extend(Ex) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:3>>, S = extend(Ex),
   <<0:1,1:2,11:5,1:3,Rm/bitstring,S/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

% C6.2.8 ADDS (shifted register)

adds(R1,R2,R3,Sh,Im) when ?x(R1), ?x(R2), ?x(R3), ?imm6(Im), ?sh3(Sh) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:6>>, S = shift(Sh),
   <<1:1,1:2,11:5,S/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>;

adds(R1,R2,R3,Sh,Im) when ?w(R1), ?w(R2), ?w(R3), ?imm6(Im), ?sh3(Sh) ->
   Rm = reg(R3), Rn = reg(R2), Rd = reg(R1), I = <<Im:6>>, S = shift(Sh),
   <<0:1,1:2,11:5,S/bitstring,0:1,Rm/bitstring,I/bitstring,Rn/bitstring,Rd/bitstring>>.

% C6.2.7 ADDS (immediate)

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

% C6.2.13 ANDS (immediate)

ands(R1,R2,Im) when ?x(R1), ?x(R2), ?imm13(Im) ->
   Rd = reg(R1), Rn = reg(R2), Immr = <<Im:6>>, Imms = <<(Im bsr 6):6>>, H = <<(Im bsr 12):1>>, Sh = <<0:2>>,
   <<1:1,3:2,9:4,Sh/bitstring,H/bitstring,Immr/bitstring,Imms/bitstring,Rn/bitstring,Rd/bitstring>>;

ands(R1,R2,Im) when ?w(R1), ?w(R2), ?imm12(Im) ->
   Rd = reg(R1), Rn = reg(R2), Immr = <<Im:6>>, Imms = <<(Im bsr 6):6>>, Sh = <<0:2>>,
   <<0:1,3:2,9:4,Sh/bitstring,0:1,Immr/bitstring,Imms/bitstring,Rn/bitstring,Rd/bitstring>>;

% C6.2.14 ANDS (shifted register)

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

% C6.2.15, C6.2.17 ASR (register)

asr(R1,R2,R3) when ?x(R1), ?x(R2), ?x(R3) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2),
   <<1:1,0:2,13:4,6:4,Rm/bitstring,2:4,2:2,Rn/bitstring,Rd/bitstring>>;

asr(R1,R2,R3) when ?w(R1), ?w(R2), ?w(R3) ->
   Rm = reg(R3), Rd = reg(R1), Rn = reg(R2),
   <<0:1,0:2,13:4,6:4,Rm/bitstring,2:4,2:2,Rn/bitstring,Rd/bitstring>>.

% C6.2.18 AT

at(Op,X) when ?op(Op), ?x(X) ->
   R = reg(X), {Op1,M,Op2} = sys(Op),
   <<13:4,5:4,1:5,Op1/bitstring,7:4,4:3,M/bitstring,Op2/bitstring,R/bitstring>>.

% C6.2.19 AUTDA, AUTDZA

autdza(R1) when ?x(R1) ->
   <<>>.

autda(R1,R2) when ?x(R1), ?x(R2) ->
   <<>>.

% C6.2.175 MOV (wide immediate)

mov(R1,Im) when ?x(R1), ?imm16(Im) ->
   R = reg(R1), I = <<Im:16>>,
   <<1:1,2:2,37:6,0:2,I/bitstring,R/bitstring>>;

mov(R1,Im) when ?w(R1), ?imm16(Im) ->
   R = reg(R1), I = <<Im:16>>,
   <<0:1,2:2,37:6,0:2,I/bitstring,R/bitstring>>.

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

% C6.2.281 STUR

stur(R1,[R2,Im]) when ?x(R1), ?x(R2), ?imm9(Im) ->
   Rt = reg(R1), Rn = reg(R2), I = <<Im:9>>,
   <<3:2,7:3,0:6,I/bitstring,0:2,Rn/bitstring,Rt/bitstring>>;

stur(R1,[R2,Im]) when ?w(R1), ?x(R2), ?imm9(Im) ->
   Rt = reg(R1), Rn = reg(R2), I = <<Im:9>>,
   <<2:2,7:3,0:6,I/bitstring,0:2,Rn/bitstring,Rt/bitstring>>.

% C6.2.289 SUB (immediate)

sub(R1,R2,Im) when ?x(R1), ?x(R2), ?imm12(Im) ->
   Dst = reg(R1), Src = reg(R2), I = <<Im:12>>,
   <<1:1,1:1,0:1,34:6,0:1,I/bitstring,Src/bitstring,Dst/bitstring>>;

sub(R1,R2,Im) when ?w(R1), ?w(R2), ?imm12(Im) ->
   Dst = reg(R1), Src = reg(R2), I = <<Im:12>>,
   <<0:1,1:1,0:1,34:6,0:1,I/bitstring,Src/bitstring,Dst/bitstring>>.
