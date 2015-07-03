-module(p).
-export([p/0]).

%(*) Duplicate the elements of a list.
%    Example:
%    ?- dupli([a,b,c,c,d],X).
%    X = [a,a,b,b,c,c,c,c,d,d]

p1(X) ->
    p1(X, []).

p1([], Res) ->
    lists:reverse(Res);
p1([X | Tail], Res) ->
    p1(Tail, [X, X | Res]).

p2(X) ->
    p2(X, undefined, 0, []).

p2([], X, N, Res) ->
    lists:append(lists:reverse(add2(X, N, Res)));
p2([X | Tail], undefined, 0, Res) ->
    p2(Tail, X, 1, Res);
p2([X | Tail], X, N, Res) ->
    p2(Tail, X, N + 1, Res);
p2(Src, X, N, Res) ->
    p2(Src, undefined, 0, add2(X, N, Res)).

add2(X, N, Res) ->
    [lists:duplicate(N * 2, X) | Res].

p3(X) ->
    p3(X, []).

p3([], Res) ->
    lists:append(lists:reverse(Res));
p3([X | Tail], Res) ->
    p3(Tail, [lists:duplicate(2, X) | Res]).

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
    Funcs = [fun p1/1, fun p2/1, fun p3/1],
    Tests = [
    {[a,b,c,c,d], [a,a,b,b,c,c,c,c,d,d]}
    , {[], []}
    , {[1,2,3], [1,1,2,2,3,3]}
    , {[0,0,0,0], [0,0,0,0,0,0,0,0]}
    ],
    test(Funcs, Tests).
