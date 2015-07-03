-module(p).
-export([p/0]).

test(Funcs, Tests) ->
    lists:foreach(fun(F) -> test(F, Tests, true) end, Funcs).

test(Func, [{Args, Expected} | TTail], Status) ->
    Res = apply(Func, Args),
    Pid = spawn(?MODULE, Func, Args),
    %io:format("~w -> ~w vs ~w: ~w~n", [Args, Res, Expected, Res == Expected]),
    test(Func, TTail, Status and (Expected == Res));
test(Func, [], Status) ->
    %io:format("~w --- ~w~n", [Func, Status]),
    Status.

%run in separate process
funcTester(Func, Tests) ->
    io:format("Started ~w~n", [Func]),
    testSingleFunc(Func, Tests).

testSingleFunc(Func, [H | Tail]) ->
    process_flag(trap_exit, true),
    spawn_link(fun() -> performSingleTest(self(), Func, H) end),
    io:format("watinig..~n"),
    receive
        {complete, Res} ->
            io:format("Aweseom: ~w~n", [Res]);
        _X ->
            io:format("Thats my boy: ~w~n", [_X])
    end.

performSingleTest(Parent, Func, {Args, Expected}) ->
    Parent ! {complete, apply(Func, Args)}.

len(X) ->
    len(X, 0).
len([], N) ->
    N;
len([_X | Tail], N) ->
    len(Tail, N + 1).

p() ->
    Funcs = [fun length/1, fun len/1],
    Tests = [
    {[1], 1}
    , {[1,2], 2}
    , {[1,2,3], 3}
    ],
    lists:foreach(fun(Func) ->
            spawn(fun() -> funcTester(Func, Tests) end) end, Funcs).
