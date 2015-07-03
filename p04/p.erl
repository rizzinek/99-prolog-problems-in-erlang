-module(p).
-export([p1/1, p2/1]).

%find the number of elements in the list

p1(X) ->
    length(X).

p2(X) ->
    p2(X, 0).

p2([], N) ->
    N;
p2([_ | Tail], N) ->
    p2(Tail, N + 1).
