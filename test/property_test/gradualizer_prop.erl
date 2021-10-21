-module(gradualizer_prop).

-compile([export_all]).

-include_lib("proper/include/proper.hrl").

-type t() :: {atom, atom()} | {integer, integer()}.

prop_t_is_an_atom_or_an_integer() ->
    ?FORALL(Type, t(),
            is_atom_or_integer(Type)).

is_atom_or_integer({atom, At}) when is_atom(At) -> true;
is_atom_or_integer({integer, Int}) when is_integer(Int) -> true;
is_atom_or_integer(_) -> false.


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

env(Opts) ->
    Forms = [],
    ParseData = typechecker:collect_specs_types_opaques_and_functions(Forms),
    typechecker:create_env(ParseData, Opts).
