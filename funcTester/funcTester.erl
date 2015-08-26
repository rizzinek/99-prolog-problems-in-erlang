-module(funcTester).
-export([performTesting/2]).

-record (testedFunction
		, {description, funcObject}).

funcTester(F, [], _TestNumber, Results) ->
	printTestResults(F, Results);
funcTester(F, [SingleTest | Tests], TestNumber, Results) ->
	process_flag(trap_exit, true),
	Pid = self(),
	%separate process for running a single test
	%so that a single test crash doesn't bring the whole thing down
	RunnerPid = spawn_link(fun() -> performSingleTest(Pid, F#testedFunction.funcObject, SingleTest) end),
	NewResults = getTestResults(RunnerPid, TestNumber, SingleTest, Results),
	funcTester(F, Tests, TestNumber + 1, NewResults).

getTestResults(RunnerPid, TestNumber, {Args, Exp}, OldResults) ->
	receive
		{testSuccess, RunnerPid, Res} ->
			lists:append(OldResults, [io_lib:format("  Test ~B: ~s~n", [TestNumber, Res])]);
		{testFailure, RunnerPid, Res} ->
			lists:append(OldResults, [io_lib:format("  Test ~B: ~s~n", [TestNumber, Res])]);
		{'EXIT', RunnerPid, Reason} ->
			lists:append(OldResults, [io_lib:format("  Test ~B: CRASH with reason ~w~n", [TestNumber, Reason])])
	after
		4000 ->
			lists:append(OldResults, [io_lib:format("  Test ~B: TIMEOUT, f(~w) = ?, expected ~w~n", [TestNumber, Args, Exp])])
	end.

printTestResults(F, []) ->
	io:format("~s: no test results~n", [F#testedFunction.description]);
printTestResults(F, Results) ->
	io:format("~s:~n", [F#testedFunction.description]),
	printTestResults(Results).

printTestResults([]) ->
	io:format("~n");
printTestResults([H | Results]) ->
	io:format("~s", [H]),
	printTestResults(Results).

performSingleTest(Parent, Func, {Args, Expected}) ->
	Val = apply(Func, Args),
	if
		Val =:= Expected ->
			Res = io_lib:format("SUCCESS, f(~w) = ~w, expected ~w", [lists:flatten(Args), Val, Expected]),
			Parent ! {testSuccess, self(), Res};
		true ->
			Res = io_lib:format("FAILURE, f(~w) = ~w, expected ~w", [lists:flatten(Args), Val, Expected]),
			Parent ! {testFailure, self(), Res}
	end.

performTesting(TestedFuncs, Tests) ->
	lists:foreach(fun(F) ->
		funcTester(F, Tests, 1, []) end, TestedFuncs).
