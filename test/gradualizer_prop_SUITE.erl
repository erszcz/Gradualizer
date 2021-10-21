-module(gradualizer_prop_SUITE).

-compile([export_all, nowarn_export_all]).

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
    ct_property_test:quickcheck(gradualizer_prop:prop_remove_pos_removes_pos(), Config).

atom_or_integer(Config) ->
    ct_property_test:quickcheck(gradualizer_prop:prop_t_is_an_atom_or_an_integer(), Config).

normalize_type(Config) ->
    ct_property_test:quickcheck(proper:numtests(100, gradualizer_prop:prop_normalize_type()), Config).
