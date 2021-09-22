-module(maps_reached).

-type t()  :: {}.

%% Map: Key {} and value {} are of type t()
-spec map(#{t() => t()}) -> boolean().
map(#{{} := {}}) -> true;
map(#{})         -> false.
