-module(exhaustive_fun_with_multiple_params).

-export([f/2]).

-type adt() :: a | b.

-spec f(adt(), _) -> ok.
f(a, _) -> ok.
