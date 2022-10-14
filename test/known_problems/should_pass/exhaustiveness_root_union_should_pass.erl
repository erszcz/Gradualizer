-module(exhaustiveness_root_union_should_pass).

-export([g/0]).

-spec f() -> ok | {error, foo} | {error, bar}.
f() -> ok.

-spec g() -> number().
g() ->
    case f() of
        ok -> 42;
        {error, _Sth} -> 43
    end.
