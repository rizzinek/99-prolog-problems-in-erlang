-module(p).
-export([p1/1, p2/1]).

%reverse a list

p1(X) ->
    lists:reverse(X).

p2(X) ->
    p2(X, []).

p2([], Acc) ->
    Acc;
p2([X | Tail], Acc) ->
    p2(Tail, [X | Acc]).
