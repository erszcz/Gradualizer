-module(rec).

-export([f/0]).

-record(r, {a :: integer()}).
-record(s, {a :: float()}).

-spec f() -> #s{}.
f() ->
    S = #s{},
    S#s{a = asd}.
