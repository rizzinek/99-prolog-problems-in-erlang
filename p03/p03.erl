% (*) Find the K'th element of a list.
% The first element in the list is number 1.
% Example:
% ?- element_at(X,[a,b,c,d,e],3).
% X = c

-module(p03).
-export([p/0]).

-include("../funcTester/funcTester.hrl").

%find the nth element of a list

p1([X | _], 1) ->
    X;
p1([_ | Tail], N) ->
    p1(Tail, N - 1).

p2(X, N) ->
    lists:nth(N, X).

p() ->
	code:add_path("../funcTester"),
	code:load_file(funcTester),
	Funcs = [
	#testedFunction{description="my tail-recursive version of finding the nth element", funcObject=fun p1/2}
	, #testedFunction{description="lists:nth", funcObject=fun p2/2}
	],
	Tests = [
	#singleTest{arguments = [[a, b, c, d], 3], expectedValue = c}
	, #singleTest{arguments = [[], 3], expectedValue = error}
	, #singleTest{arguments = [[123], 0], expectedValue = error}
	, #singleTest{arguments = [[123, 456], 1], expectedValue = 123}
	, #singleTest{arguments = [[123, 456], 5], expectedValue = error}
	],
	funcTester:performTesting(Funcs, Tests).
