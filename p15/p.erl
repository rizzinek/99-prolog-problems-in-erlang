-module(p).
-export([p/0]).

%(*) Duplicate the elements of a list.
%    Example:
%    ?- dupli([a,b,c,c,d],X).
%    X = [a,a,b,b,c,c,c,c,d,d]

p1(X, Coef) ->
    p1(X, Coef, []).

p1([], _Coef, Res) ->
    lists:reverse(Res);
p1([X | Tail], Coef, Res) ->
    p1(Tail, Coef, add1(Res, X, Coef)).

add1(Dst, _X, 0) ->
    Dst;
add1(Dst, X, Num) ->
    add1([X | Dst], X, Num - 1).

p2(X, Coef) ->
    p2(X, Coef, undefined, 0, []).

p2([], Coef, X, N, Res) ->
    lists:append(lists:reverse(add2(X, N, Coef, Res)));
p2([X | Tail], Coef, undefined, 0, Res) ->
    p2(Tail, Coef, X, 1, Res);
p2([X | Tail], Coef, X, N, Res) ->
    p2(Tail, Coef, X, N + 1, Res);
p2(Src, Coef, X, N, Res) ->
    p2(Src, Coef, undefined, 0, add2(X, N, Coef, Res)).

add2(X, N, Coef, Res) ->
    [lists:duplicate(N * Coef, X) | Res].

p3(X, Coef) ->
    p3(X, Coef, []).

p3([], _Coef, Res) ->
    lists:append(lists:reverse(Res));
p3([X | Tail], Coef, Res) ->
    p3(Tail, Coef, [lists:duplicate(Coef, X) | Res]).

test(Funcs, Tests) ->
    lists:foreach(fun(F) -> test(F, Tests, true) end, Funcs).

test(Func, [{Args, Expected} | TTail], Status) ->
    Res = apply(Func, Args),
    io:format("~w -> ~w vs ~w: ~w~n", [Args, Res, Expected, Res == Expected]),
    test(Func, TTail, Status and (Expected == Res));
test(Func, [], Status) ->
    io:format("~w --- ~w~n", [Func, Status]),
    Status.

p() ->
    Funcs = [fun p1/2, fun p2/2, fun p3/2],
    Tests = [
    {[[a,b,c,c,d], 2], [a,a,b,b,c,c,c,c,d,d]}
    , {[[], 5], []}
    , {[[1,2,3], 3], [1,1,1,2,2,2,3,3,3]}
    , {[[0,0,0,0], 1], [0,0,0,0]}
    , {[[a,1,4,g], 0], []}
    ],
    test(Funcs, Tests).
