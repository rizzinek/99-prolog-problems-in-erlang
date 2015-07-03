-module(p).
-export([p/0]).

%(**) Flatten a nested list structure.
%    Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
%
%    Example:
%    ?- my_flatten([a, [b, [c, d], e]], X).
%    X = [a, b, c, d, e]
%
%    Hint: Use the predefined predicates is_list/1 and append/3

p1(X) ->
    lists:flatten(X).

p2(X) ->
    lists:reverse(p2(X, [])).
p2([X | Tail], Acc) when is_list(X) ->
    p2(Tail, p2(X, Acc));
p2([X | Tail], Acc) ->
    p2(Tail, [X | Acc]);
p2([], Acc) ->
    Acc.

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
    Funcs = [fun p1/1, fun p2/1],
    Tests = [
    {[[1], [2], [3, 4, [5, 6]]], [1, 2, 3, 4, 5, 6]}
    , {[[1], [2], [3]], [1, 2, 3]}
    , {[1, 2, 3, 4], [1, 2, 3, 4]}
    ],
    test(Funcs, Tests).
