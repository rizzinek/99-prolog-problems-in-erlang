-module(p).
-export([p/0]).

%(**) Drop every N'th element from a list.
%    Example:
%    ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
%    X = [a,b,d,e,g,h,k]

p1(X, N) ->
    p1(X, N, 1, []).

p1(X, 0, _Count, _Res) ->
    X;
p1([], _N, _Count, Res) ->
    lists:reverse(Res);
p1([X | Tail], N, Count, Res) when Count < N ->
    p1(Tail, N, Count + 1, [X | Res]);
p1([_X | Tail], N, Count, Res) when Count =:= N ->
    p1(Tail, N, 1, Res).

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
    Funcs = [fun p1/2],
    Tests = [
    {[[a,b,c,d,e,f,g,h,i,k], 3], [a,b,d,e,g,h,k]}
    , {[[a,b,c,d,e], 0], [a,b,c,d,e]}
    , {[[], 5], []}
    , {[[1,1,2,2,3,3], 2], [1,2,3]}
    , {[[a,b,c,d], 1], []}
],
    test(Funcs, Tests).
