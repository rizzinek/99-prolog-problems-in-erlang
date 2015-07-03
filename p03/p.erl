-module(p).
-export([p1/2, p2/2]).

%find the nth element of a list

p1(X, N) ->
    lists:nth(N, X).

p2([X | _], 1) ->
    X;
p2([_ | Tail], N) ->
    p2(Tail, N - 1).
