% (*) Find the last but one element of a list.

-module(p02).
-export([p/0]).

-include("../funcTester/funcTester.hrl").

%find the second to last element of a list

p1([X | [_]]) ->
    X;
p1([_ | Tail]) ->
    p1(Tail).

p() ->
	code:add_path("../funcTester"),
	code:load_file(funcTester),
	Funcs = [
	#testedFunction{description="my tail-recursive version of finding the second to last element", funcObject=fun p1/1}
	, #testedFunction{description="lists:nth(lists:reverse)", funcObject=fun(X) -> lists:nth(2, lists:reverse(X)) end}
	, #testedFunction{description="hd(lists:nthtail)", funcObject=fun(X) -> hd(lists:nthtail(length(X) - 2, X)) end}],
	Tests = [
	#singleTest{arguments = [[a, b, c, d]], expectedValue = c}
	, #singleTest{arguments = [[]], expectedValue = error}
	, #singleTest{arguments = [[123]], expectedValue = error}
	, #singleTest{arguments = [[123, 456]], expectedValue = 123}
	],
	funcTester:performTesting(Funcs, Tests).
