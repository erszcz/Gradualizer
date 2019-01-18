-module(map_field_valid_update).

-export([f/1]).

-spec f(#{a := integer(), b => float()}) -> #{a := binary(), b => float()}.
f(#{} = Ctx) ->
    (#{})#{a := <<"binary">>}.
