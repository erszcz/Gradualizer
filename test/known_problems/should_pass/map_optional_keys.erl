-module(map_optional_keys).

-export([m/0, m2/0]).

-spec m() -> #{foo := atom(), bar => optional}.
m() ->
    #{foo => bar}.

-spec m2() -> #{bar => optional}.
m2() ->
    #{foo => bar}.
