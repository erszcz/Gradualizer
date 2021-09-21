-module(maps).

-export([map_variants/1]).
-export_type([map_sum_t/0]).

-spec f(#{a := boolean()}) -> #{a := integer()}.
f(#{} = Ctx) ->
    Ctx#{a := 5}.

-type map_sum_t() :: #{field_one := integer()}
                   | #{field_two := integer()}.

-spec map_variants(map_sum_t()) -> {ok, any()}.
map_variants(T) ->
    case T of
        #{field_one := A} -> {ok, A}
    end.
