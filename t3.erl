-module(t3).

-export([p/2]).

-spec p(A, A) -> A.
p(A, B) -> A + B.

-spec test() -> integer().
test() ->
    p(1, 3.2).
