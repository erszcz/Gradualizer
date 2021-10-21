-module(gradualizer_prop_SUITE).

-compile([export_all, nowarn_export_all]).

-define(NUMTESTS, list_to_integer(os:getenv("PROP_NUMTESTS", "100"))).

%% Aliases
-define(cpt, ct_property_test).
-define(gp, gradualizer_prop).

all() ->
    [
     remove_pos_removes_pos,
     atom_or_integer,
     normalize_type,
     glb
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    {ok, _} = application:ensure_all_started(gradualizer),
    Config.

end_per_testcase(_CaseName, Config) ->
    %% Clear gradualizer_db data between test runs.
    ok = application:stop(gradualizer),
    Config.

remove_pos_removes_pos(Config) ->
    ?cpt:quickcheck(proper:numtests(?NUMTESTS, ?gp:prop_remove_pos_removes_pos()), Config).

atom_or_integer(Config) ->
    ?cpt:quickcheck(proper:numtests(?NUMTESTS, ?gp:prop_t_is_an_atom_or_an_integer()), Config).

normalize_type(Config) ->
    ?cpt:quickcheck(proper:numtests(?NUMTESTS, ?gp:prop_normalize_type()), Config).

glb(Config) ->
    ?cpt:quickcheck(proper:numtests(?NUMTESTS, ?gp:prop_glb()), Config).
