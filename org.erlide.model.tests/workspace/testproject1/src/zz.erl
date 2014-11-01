-module(yy2).
-export([a/1, b/0]).

a(L) when is_list(L)->
    lists:reverse(L);
a(A) when is_atom(A)->
    atom_to_list(A).
b() ->
    b.
% make a tuple
c(A, B, C) ->
    {c, A, B, C}.
