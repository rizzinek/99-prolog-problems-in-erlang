-module(p).
-export([p/0]).

%(**) Pack consecutive duplicates of list elements into sublists.
%    If a list contains repeated elements they should be placed in separate sublists.
%
%    Example:
%    ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]

p1(X) ->
    p1(X, [], []).

p1([], [], Res) ->
    Res;
p1([], Tmp, Res) ->
    lists:reverse([Tmp | Res]);
p1([X | Tail], [], Res) ->
    p1(Tail, [X], Res);
p1([X | SrcTail], [X | TmpTail], Res) ->
    p1(SrcTail, [X, X | TmpTail], Res);
p1([X | SrcTail], [Y | TmpTail], Res) ->
    p1([X | SrcTail], [], [[Y | TmpTail] | Res]).

test(Funcs, Tests) ->
    lists:foreach(fun(F) -> test(F, Tests, true) end, Funcs).

test(Func, [{Arg, Expected} | TTail], Status) ->
    Res = Func(Arg),
    io:format("~w -> ~w vs ~w: ~w~n", [Arg, Res, Expected, Res == Expected]),
    test(Func, TTail, Status and (Expected == Res));
test(Func, [], Status) ->
    io:format("~w --- ~w~n", [Func, Status]),
    Status.

p() ->
    Funcs = [fun p1/1],
    Tests = [
    {[a,a,a,a,b,c,c,a,a,d,e,e,e,e], [[a,a,a,a], [b], [c,c], [a,a], [d], [e,e,e,e]]}
    , {[1,2,3,4,5], [[1],[2],[3],[4],[5]]}
    , {[], []}
    , {[3,3,3,2,2,1,a,a,a,a,a], [[3,3,3], [2,2], [1], [a,a,a,a,a]]}
    ],
    test(Funcs, Tests).
