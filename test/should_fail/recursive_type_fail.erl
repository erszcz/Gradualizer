-module(recursive_type_fail).

-export([f/0]).

-type recursive_t() :: #{key => recursive_t()}.

-spec f() -> recursive_t().
f() ->
    g().

-spec g() -> recursive_t().
g() ->
    [].
