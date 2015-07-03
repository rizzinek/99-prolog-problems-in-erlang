-module(p).
-export([p1/1, p2/1, p3/1]).

%find the second to last element of a list

p1([X | [_]]) ->
    X;
p1([_ | Tail]) ->
    p1(Tail).

p2(X) ->
    lists:nth(2, lists:reverse(X)).

p3(X) ->
    hd(lists:nthtail(length(X) - 2, X)).
