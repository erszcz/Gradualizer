-module(gradualizer_config).

-export([set_union_size_limit/1]).

set_union_size_limit(Opts) ->
    case lists:keyfind(union_size_limit, 1, Opts) of
        false -> ok;
        {union_size_limit, L} ->
            persistent_term:put(gradualizer_union_size_limit, L)
    end.
