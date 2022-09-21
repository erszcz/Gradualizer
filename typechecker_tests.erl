%% @doc This is an eunit test suite. Run the tests using
%%      eunit:test(typechecker, [verbose]).
-module(typechecker_tests).

-include_lib("eunit/include/eunit.hrl").

%% Macro to convert type to abstract form
-define(t(T), typelib:remove_pos(typelib:parse_type(??T))).

subtype_test_() ->
    [
     %% The unknown type, both directions
     ?_assert(subtype(?t( any()             ), ?t( 1..10            ))),
     ?_assert(subtype(?t( 1..10             ), ?t( any()            ))),

     %% Term and none
     ?_assert(subtype(?t( sets:set()        ), ?t( term()           ))),
     ?_assert(subtype(?t( none()            ), ?t( sets:set()       ))),

     %% Integer
     ?_assert(subtype(?t( 1                 ), ?t( integer()        ))),
     ?_assert(subtype(?t( 1..5              ), ?t( integer()        ))),
     ?_assert(subtype(?t( 1..5              ), ?t( 1..10            ))),
     ?_assert(subtype(?t( 2                 ), ?t( 1..10            ))),

     %% Number
     ?_assert(subtype(?t( 1                 ), ?t( number()         ))),
     ?_assert(subtype(?t( 1..5              ), ?t( number()         ))),
     ?_assert(subtype(?t( integer()         ), ?t( number()         ))),
     ?_assert(subtype(?t( float()           ), ?t( number()         ))),

     %% Atom
     ?_assert(subtype(?t( a                 ), ?t( atom()           ))),
     ?_assert(subtype(?t( a                 ), ?t( a                ))),

     %% Union
     ?_assert(subtype(?t( b                 ), ?t( a|b              ))),
     ?_assert(subtype(?t( 1|a               ), ?t( integer()|atom() ))),
     ?_assert(subtype(?t( 1..5              ), ?t( 1..3|4..6        ))),
     ?_assert(subtype(?t( 1..5|a            ), ?t( 1..3|4..6|atom() ))),

     %% Lists
     ?_assert(subtype(?t( [a]               ), ?t( list()           ))),
     ?_assert(subtype(?t( list()            ), ?t( [a]              ))),
     ?_assert(subtype(?t( []                ), ?t( list()           ))),
     ?_assert(subtype(?t( []                ), ?t( [a]              ))),
     ?_assert(subtype(?t( [1]               ), ?t( [1..5]           ))),
     ?_assert(subtype(?t( nonempty_list()   ), ?t( list()           ))),
     ?_assert(subtype(?t( nonempty_list()   ), ?t( [a]              ))),
     ?_assert(subtype(?t( nonempty_list()   ), ?t( [a, ...]         ))),
     ?_assert(subtype(?t( [a, ...]          ), ?t( [a]              ))),

     %% Tuples
     ?_assert(subtype(?t( {a,b,c}           ), ?t( tuple()          ))),
     ?_assert(subtype(?t( tuple()           ), ?t( {a,b,c}          ))),
     ?_assert(subtype(?t( {x, 1}            ), ?t( {atom(), 1..5}   ))),

     %% Maps
     ?_assert(subtype(?t( map()             ), ?t( #{a := b}        ))),
     ?_assert(subtype(?t( #{a := b}         ), ?t( map()            ))),
     ?_assert(subtype(?t( #{a => b}         ), ?t( #{}              ))),
     ?_assert(subtype(?t( #{a := b}         ), ?t( #{a => b}        ))),
     ?_assert(subtype(?t( #{1..5 := a }     ), ?t( #{5 := atom()}   )))
    ].

not_subtype_test() ->
    [
     %% Numeric
     ?_assertNot(subtype(?t( 1              ), ?t( 2                ))),
     ?_assertNot(subtype(?t( integer()      ), ?t( 1                ))),
     ?_assertNot(subtype(?t( integer()      ), ?t( 1..5             ))),
     ?_assertNot(subtype(?t( 1..10          ), ?t( 1                ))),
     ?_assertNot(subtype(?t( 1..10          ), ?t( 1..5             ))),
     ?_assertNot(subtype(?t( integer()      ), ?t( float()          ))),
     ?_assertNot(subtype(?t( float()        ), ?t( integer()        ))),
     ?_assertNot(subtype(?t( number()       ), ?t( integer()        ))),
     ?_assertNot(subtype(?t( number()       ), ?t( float()          ))),

     %% Atom
     ?_assertNot(subtype(?t( a              ), ?t( b                ))),

     %% Union
     ?_assertNot(subtype(?t( a|b            ), ?t( b                ))),

     %% Lists
     ?_assertNot(subtype(?t( []             ), ?t( nonempty_list()  ))),
     ?_assertNot(subtype(?t( []             ), ?t( [a, ...]         ))),
     ?_assertNot(subtype(?t( list()         ), ?t( nonempty_list()  ))),
     ?_assertNot(subtype(?t( [a]            ), ?t( nonempty_list()  ))),
     ?_assertNot(subtype(?t( [a]            ), ?t( [a, ...]         ))),
     ?_assertNot(subtype(?t( [b]            ), ?t( [a]              ))),

     %% Tuples
     ?_assertNot(subtype(?t( {}             ), ?t( {any()}          ))),
     ?_assertNot(subtype(?t( {1..2, 3..4}   ), ?t( {1, 3}           ))),

     %% Maps
     ?_assertNot(subtype(?t( #{}            ), ?t( #{a := b}        ))),
     ?_assertNot(subtype(?t( #{a => b}      ), ?t( #{a := b}        ))),
     ?_assertNot(subtype(?t( #{a := 1..5}   ), ?t( #{a := 2}        ))),
     ?_assertNot(subtype(?t( #{1 := atom()} ), ?t( #{1 := a}        )))
    ].

normalize_test_() ->
    [
     {"Merge intervals",
      ?_assertEqual(?t( 1..6 ),
                    typechecker:normalize(?t( 1..3|4..6 ), test_lib:create_env([])))},
     {"Remove singleton atoms if atom() is present",
      ?_assertEqual(?t( atom() ),
                    typechecker:normalize(?t( a | atom() | b ), test_lib:create_env([])))},
     {"Evaluate numeric operators in types",
      %% ?t(-8) is parsed as {op,0,'-',{integer,0,8}}
      ?_assertEqual({integer, 0 , -8},
                    typechecker:normalize(?t( (bnot 3) *
                                              (( + 7 ) rem ( 5 div - 2 ) ) bxor
                                              (1 bsl 6 bsr 4) ), test_lib:create_env([])))},
     {"normalize(boolean()) == normalize(normalize(boolean))",
      ?_assertEqual(typechecker:normalize(?t( boolean() ), test_lib:create_env([])),
                    typechecker:normalize(typechecker:normalize(?t( boolean() ),
                                                                test_lib:create_env([])),
                                          test_lib:create_env([])))
     },
     {"normalize(union()) == normalize(normalize(union()))",
      ?_test(begin
                 gradualizer_db:start_link(),
                 gradualizer_db:import_files(["env.erl"]),
                 ?assertEqual( typechecker:normalize(?t( env:t1() )),
                               typechecker:normalize(typechecker:normalize(?t( env:t1() ))) )
             end)}
    ].

subtype(T1, T2) ->
    case typechecker:subtype(T1, T2) of
	{true, _} ->
	    true;
	false ->
	    false
    end.
