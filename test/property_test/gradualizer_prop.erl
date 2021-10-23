-module(gradualizer_prop).

-compile([export_all]).

-include_lib("common_test/include/ct_property_test.hrl").

expr() ->
    gradualizer_type_gen:expr().

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
    Type1_ = typechecker:normalize(Type1, Env),
    Type2_ = typechecker:normalize(Type2, Env),
    typechecker:glb(Type1_, Type2_, Env),
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

prop_type_diff() ->
    ?FORALL({Type1, Type2},
            {abstract_type(), abstract_type()},
            ?TIMEOUT(timer:seconds(1),
                     prop_type_diff_(Type1, Type2))).

prop_type_diff_(Type1, Type2) ->
    Env = env([]),
    typechecker:type_diff(Type1, Type2, Env),
    %% we're only interested in termination / infinite recursion
    true.

%% TODO: Actually, directly checking refinable might not be necessary in the long run,
%%       since it's called from type_check_expr_in that we should have a prop for anyway.
prop_refinable() ->
    ?FORALL(Type,
            {abstract_type(), abstract_type()},
            ?TIMEOUT(timer:seconds(1),
                     prop_refinable_(Type))).

prop_refinable_(Type) ->
    Env = env([]),
    typechecker:refinable(Type, Env),
    %% we're only interested in termination / infinite recursion
    true.

prop_compatible() ->
    ?FORALL({Type1, Type2},
            {abstract_type(), abstract_type()},
            ?TIMEOUT(timer:seconds(1),
                     prop_compatible_(Type1, Type2))).

prop_compatible_(Type1, Type2) ->
    Env = env([]),
    Type1_ = typelib:remove_pos(Type1),
    Type2_ = typelib:remove_pos(Type2),
    typechecker:compatible(Type1_, Type2_, Env),
    %% we're only interested in termination / infinite recursion
    true.

prop_type_check_expr() ->
    ?FORALL(Expr, expr(),
            ?WHENFAIL(ct:pal("~s failed:\n~p\n", [?FUNCTION_NAME, Expr]),
                      ?TIMEOUT(timer:seconds(1),
                               prop_type_check_expr_(Expr)))).

prop_type_check_expr_(Expr) ->
    Env = env([]),
    typechecker:type_check_expr(Env, Expr),
    %% we're only interested in termination / infinite recursion / crashes
    true.

%% TODO: prop_ add_type_pat - ultimately called from type_check_expr_in; requires a pattern() gen
%% TODO: prop_ type_check_expr_in, unless the last two are merged, requires an expr() gen,
%%       ultimately type_check_expr_in is called from type_check_expr
%% TODO: prop_ type_check_forms - this one will actually subsume all of the above if we devise a good
%%       enough generator; requires a form() generator

env(Opts) ->
    Forms = [],
    ParseData = typechecker:collect_specs_types_opaques_and_functions(Forms),
    typechecker:create_env(ParseData, Opts).
