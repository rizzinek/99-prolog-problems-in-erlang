-module(p).
-export([p/0]).

%(**) Eliminate consecutive duplicates of list elements.
%    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
%
%    Example:
%    ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [a,b,c,a,d,e]

p1(X) ->
    p1(X, []).

p1([X, X | Tail], Acc) ->
    p1([X | Tail], Acc);
p1([X | Tail], Acc) ->
    p1(Tail, [X | Acc]);
p1([], Acc) ->
    lists:reverse(Acc).

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
    {[a,a,a,a,b,c,c,a,a,d,e,e,e,e], [a,b,c,a,d,e]}
    , {[1,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,4], [1,2,3,4]}
    , {[1,2,3,4,5, a], [1,2,3,4,5, a]}
    ],
    test(Funcs, Tests).
