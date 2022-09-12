-module(recursive_type_fail).

-export([g/0, i/0, j/0]).

-type recursive_t1() :: #{key => recursive_t1()}.

-type recursive_t2() :: #{binary() | atom() => recursive_t2()} | true | false | null.

-spec f() -> recursive_t1().
f() ->
    g().

-spec g() -> recursive_t1().
g() ->
    [].

-spec i() -> recursive_t2().
i() ->
    j().

-spec j() -> recursive_t2().
j() ->
    [].
