%(*) Find the last element of a list.
%Example:
%?- my_last(X,[a,b,c,d]).
%X = d

-module(p01).
-export([p/0]).

-include("../funcTester/funcTester.hrl").

%finds the last element of the list
last([X]) ->
    X;
last([_ | Tail]) ->
    last(Tail).

p() ->
	code:add_path("../funcTester"),
	code:load_file(funcTester),
	Funcs = [
	#testedFunction{description="my tail-recursive version of finding the last element", funcObject=fun last/1}
	, #testedFunction{description="lists:last", funcObject=fun lists:last/1}
	, #testedFunction{description="hd(lists:reverse)", funcObject=fun(X) -> hd(lists:reverse(X)) end}],
	Tests = [
	#singleTest{arguments = [[a, b, c, d]], expectedValue = d}
	, #singleTest{arguments = [[]], expectedValue = error}
	, #singleTest{arguments = [123], expectedValue = error}
	, #singleTest{arguments = [[123]], expectedValue = 123}
	],
	funcTester:performTesting(Funcs, Tests).
	
