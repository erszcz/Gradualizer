-module(gradualizer_prop).

-compile([export_all]).

-include_lib("proper/include/proper.hrl").

abstract_type() ->
    gradualizer_type_gen:abstract_type().

prop_remove_pos_removes_pos() ->
    ?FORALL(Type, abstract_type(),
            ?WHENFAIL(ct:pal("~s failed:\n~p\n", [?FUNCTION_NAME, Type]),
                      prop_remove_pos_removes_pos_(Type))).

prop_remove_pos_removes_pos_(Type) ->
    typelib:remove_pos(Type),
    %% we're only interested in termination / infinite recursion for now
    true.

prop_normalize_type() ->
    ?FORALL(Type,
            abstract_type(),
            ?WHENFAIL(ct:pal("~s failed:\n~p\n", [?FUNCTION_NAME, Type]),
                      ?TIMEOUT(timer:seconds(1),
                               begin
                                   %mock_type_in_gradualizer_db(Type),
                                   prop_normalize_type_(Type)
                               end))).

%% TODO: First, this only catches `user_type' on the top-level, not when it's generated
%%       as a nested type.
%%       Second, we should come up with something more clever to handle user type generation.
%%       Maybe predefine the type in gradualizer_db in such a case?
%is_not_user_type({user_type, _, _, _}) -> false;
%is_not_user_type(_) -> true.

%mock_type_in_gradualizer_db(Type) ->
%    ct:pal("~p: not_implemented_yet\n", [?FUNCTION_NAME]),
%    ok.

prop_normalize_type_(Type) ->
    Env = env([]),
    typechecker:normalize(Type, Env),
    %% we're only interested in normalize termination / infinite recursion
    true.

prop_glb() ->
    ?FORALL({Type1, Type2},
            {abstract_type(), abstract_type()},
            ?TIMEOUT(timer:seconds(1),
                     prop_glb_(Type1, Type2))).

prop_glb_(Type1, Type2) ->
    Env = env([]),
    typechecker:glb(Type1, Type2, Env),
    %% we're only interested in termination / infinite recursion
    true.

prop_int_range_to_types() ->
    ?FORALL(Range, gradualizer_int:int_range(),
            prop_int_range_to_types_(Range)).

prop_int_range_to_types_(Range) ->
    Types = gradualizer_int:int_range_to_types(Range),
    lists:all(fun gradualizer_int:is_int_type/1, Types).

prop_int_range_to_types_to_int_range() ->
    ?FORALL(Range, gradualizer_int:int_range(),
            prop_int_range_to_types_to_int_range_(Range)).

prop_int_range_to_types_to_int_range_(Range) ->
    Types = gradualizer_int:int_range_to_types(Range),
    Ranges = [ gradualizer_int:int_type_to_range(Type) || Type <- Types ],
    lists:all(fun is_valid_int_range/1, Ranges).

is_valid_int_range({I, J}) when I =< J -> true;
is_valid_int_range({neg_inf, J}) when is_integer(J) -> true;
is_valid_int_range({I, pos_inf}) when is_integer(I) -> true;
is_valid_int_range(_) -> false.

env(Opts) ->
    Forms = [],
    ParseData = typechecker:collect_specs_types_opaques_and_functions(Forms),
    typechecker:create_env(ParseData, Opts).
