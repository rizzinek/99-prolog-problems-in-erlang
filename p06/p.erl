-module(p).
-export([p1/1, p2/1]).

%(*) Find out whether a list is a palindrome.
%    A palindrome can be read forward or backward; e.g. [x,a,m,a,x].

%exception error if not a palindrome
p1(X) ->
    X = lists:reverse(X).

%true/false
p2(X) ->
    X == lists:reverse(X).
