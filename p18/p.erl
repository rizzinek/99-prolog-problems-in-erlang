-module(p).
-export([p/0]).

%(**) Extract a slice from a list.
%    Given two indices, I and K, the slice is the list containing the elements
%    between the I'th and K'th element of the original list (both limits included).
%    Start counting the elements with 1.
%
%    Example:
%    ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
%    X = [c,d,e,f,g]

p1(X, Start, End) ->
    X.
%p1(X, Start, End) ->
%    p1(X, Start, End, 1, []).

p1([], _Start, _End, _Cur, Res) ->
    lists:reverse(Res).

test(Funcs, Tests) ->
    lists:foreach(fun(F) -> test(F, Tests, true) end, Funcs).

test(Func, [{Args, Expected} | TTail], Status) ->
    %Res = apply(Func, Args),
    Res = spawn(p, Func, Args),
    io:format("~w -> ~w vs ~w: ~w~n", [Args, Res, Expected, Res == Expected]),
    test(Func, TTail, Status and (Expected == Res));
test(Func, [], Status) ->
    io:format("~w --- ~w~n", [Func, Status]),
    Status.

p() ->
    %Funcs = [fun p1/3],
    Funcs = [p1],
    Tests = [
    {[[a,b,c,d,e,f,g,h,i,k], 3, 7], [c,d,e,f,g]}
    , {[[], 1, 1], []}
    , {[[1,2,3], 2, 5], [2,3]}
    , {[[1,2,3], 0, 2], [1,2]}
    ],
    test(Funcs, Tests).
