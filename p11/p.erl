-module(p).
-export([p/0]).

%(*) Modified run-length encoding.
%    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as [N,E] terms.
%
%    Example:
%    ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [[4,a],b,[2,c],[2,a],d,[4,e]]

%first pack the whole list using the function from p09
%then encode it
pack(X) ->
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

p1(X) ->
    encode1(pack(X), []).

encode1([], Acc) ->
    lists:reverse(Acc);
encode1([[Sym] | Tail], Acc) ->
    encode1(Tail, [Sym | Acc]);
encode1([[Sym | Syms] | Tail], Acc) ->
    encode1(Tail, [[length(Syms) + 1, Sym] | Acc]).

%encode in one pass
p2(X) ->
    p2(X, undefined, 0, []).

p2([], undefined, 0, Res) ->
    Res;
p2([], X, N, Res) ->
    lists:reverse(add2(X, N, Res));
p2([X | Tail], undefined, 0, Res) ->
    p2(Tail, X, 1, Res);
p2([X | Tail], X, N, Res) ->
    p2(Tail, X, N + 1, Res);
p2([Y | Tail], X, N, Res) ->
    p2([Y | Tail], undefined, 0, add2(X, N, Res)).

add2(X, N, Res) when N > 1 ->
    [[N, X] | Res];
add2(X, _, Res) ->
    [X | Res].

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
    {[a,a,a,a,b,c,c,a,a,d,e,e,e,e], [[4,a], b, [2,c], [2,a], d, [4,e]]}
    , {[1,2,3,4,5], [1,2,3,4,5]}
    , {[], []}
    , {[3,3,3,2,2,1,a,a,a,a,a], [[3,3], [2,2], 1, [5,a]]}
    ],
    test(Funcs, Tests).
