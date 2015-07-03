-module(p).
-export([p/0]).

%(**) Decode a run-length encoded list.
%    Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.

p1(X) ->
    p1(X, []).

p1([], Res) ->
    lists:append(lists:reverse(Res));
p1([[N, X] | Tail], Res) ->
    p1(Tail, [lists:duplicate(N, X) | Res]);
p1([X | Tail], Res) ->
    p1(Tail, [[X] | Res]).

p2(X) ->
    p2(X, []).

p2([], Res) ->
    lists:reverse(Res);
p2([[0, _] | Tail], Res) ->
    p2(Tail, Res);
p2([[N, X] | Tail], Res) ->
    p2([[N - 1, X] | Tail], [X | Res]);
p2([X | Tail], Res) ->
    p2(Tail, [X | Res]).

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
    {[[4,a], b, [2,c], [2,a], d, [4,e]], [a,a,a,a,b,c,c,a,a,d,e,e,e,e]}
    , {[1,2,3,4,5], [1,2,3,4,5]}
    , {[], []}
    , {[[3,3], [2,2], 1, [5,a]], [3,3,3,2,2,1,a,a,a,a,a]}
    ],
    test(Funcs, Tests).
