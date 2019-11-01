-module(update_map_type_error).

-compile(export_all).

-spec t(list()) -> map().
t(Rows) ->
    lists:foldl(fun(Id, List) ->
                        List#{id := Id}
                end, #{id => undefined}, Rows).
