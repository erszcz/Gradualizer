-module(non_neg_plus_pos_is_pos_should_pass).

-export([g/1]).

-spec g(non_neg_integer()) -> pos_integer().
g(N) ->
    h(1 + N).

-spec h(pos_integer()) -> pos_integer().
h(P) -> P.
