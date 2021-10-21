-module(gradualizer_prop_SUITE).

-compile([export_all, nowarn_export_all]).

%% Aliases
-define(cpt, ct_property_test).
-define(gp, gradualizer_prop).

all() ->
    [
     remove_pos_removes_pos,
     atom_or_integer,
     normalize_type
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

remove_pos_removes_pos(Config) ->
    ?cpt:quickcheck(?gp:prop_remove_pos_removes_pos(), Config).

atom_or_integer(Config) ->
    ?cpt:quickcheck(?gp:prop_t_is_an_atom_or_an_integer(), Config).

normalize_type(Config) ->
    ?cpt:quickcheck(proper:numtests(100, ?gp:prop_normalize_type()), Config).
