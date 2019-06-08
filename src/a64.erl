-module(a64).
-copyright('Maxim Sokhatsky').
-include("asm.hrl").
-compile(export_all).

mode()       -> 64.
main([F])    -> {ok,I} = file:read_file(F), {C,O} = compile(code(I)), file:write_file(base(F),O), halt(C);
main(_)      -> io:format("usage: a64 <file>\n"), halt(1).
base(X)      -> filename:basename(X,filename:extension(X)).
atom("#"++X) -> list_to_integer(X);
atom(X)      -> list_to_atom(X).

last(X,Y,A)  ->
  case lists:reverse(X) of
      "]"++Z -> [lists:reverse([atom(lists:reverse(Z))|Y])|A];
           _ -> {[atom(X)|Y],A} end.

code(Bin) ->
  [ list_to_tuple(
    lists:reverse(
    lists:foldl(
      fun([$[|X],A) -> last(X,[],A);
         (X,{Y,A}) -> last(X,Y,A);
         (X,A) -> [atom(X)|A] end,[],string:tokens(C," ,"))))
   || C <- string:tokens(binary_to_list(Bin),"\n") ].

compile([Code|_]) ->
   Instr = tuple_to_list(Code),
   {0,erlang:apply(?MODULE,hd(Instr),tl(Instr))}.

sub(R1,R2,Im) when ?reg(R1,R2) andalso ?imm(Im) ->
   io:format("SUB ~p, ~p, ~p~n",[R1,R2,Im]),
   Dst = reg(R1), Src = reg(R2), I = <<Im:12>>,
   <<1:1,1:1,0:1,34:6,0:1,I/bitstring,Dst/bitstring,Src/bitstring>>.
