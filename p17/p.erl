-module(p).
-export([p/0]).

%(*) Split a list into two parts; the length of the first part is given.
%    Do not use any predefined predicates.
%
%    Example:
%    ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
%    L1 = [a,b,c]
%    L2 = [d,e,f,g,h,i,k]

p1(X, N) ->
    p1(X, N, []).

p1([], _, L) ->
    {lists:reverse(L), []};
p1(X, 0, L) ->
    {lists:reverse(L), X};
p1([X | Tail], N, L1) ->
    p1(Tail, N - 1, [X | L1]).

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
    {[[a,b,c,d,e,f,g,h,i,k], 3], {[a,b,c], [d,e,f,g,h,i,k]}}
    , {[[], 5], {[], []}}
    , {[[1,2,3], 0], {[], [1,2,3]}}
    , {[[1,2,3], 3], {[1,2,3], []}}
    ],
    test(Funcs, Tests).
