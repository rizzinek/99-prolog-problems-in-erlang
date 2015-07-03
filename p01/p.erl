-module(p).
-export([p1/1, p2/1, p3/1]).

%finds the last element of the list
p1([X]) ->
    X;
p1([_ | Tail]) ->
    p1(Tail).

p2(X) ->
    lists:last(X).

p3(X) ->
    hd(lists:reverse(X)).
