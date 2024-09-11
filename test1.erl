-module(test1).
-compile(export_all).

-spec position_info_from_spec(integer() | [integer()] | none) -> number().
position_info_from_spec([_|_] = Ints) ->
    lists:sum(Ints);
position_info_from_spec(EmptyOrNone) when EmptyOrNone =:= none orelse is_list(EmptyOrNone) ->
    0;
position_info_from_spec(Int) ->
    Int.
