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


prop_remove_pos_removes_pos() ->
    ?FORALL(Type, gradualizer_type:abstract_type(),
            ?IMPLIES(is_not_user_type(Type),
                     ?WHENFAIL(ct:pal("~s failed:\n~p\n", [?FUNCTION_NAME, Type]),
                               prop_remove_pos_removes_pos_(Type)))).

prop_remove_pos_removes_pos_(Type) ->
    typelib:remove_pos(Type),
    %% we're only interested in termination / infinite recursion for now
    true.


prop_normalize_type() ->
    ?FORALL(Type,
            gradualizer_type:abstract_type(),
            ?WHENFAIL(ct:pal("~s failed:\n~p\n", [?FUNCTION_NAME, Type]),
                      ?IMPLIES(is_not_user_type(Type),
                               ?TIMEOUT(timer:seconds(1), prop_normalize_type_(Type))))).

%% TODO: First, this only catches `user_type' on the top-level, not when it's generated
%%       as a nested type.
%%       Second, we should come up with something more clever to handle user type generation.
%%       Maybe predefine the type in gradualizer_db in such a case?
is_not_user_type({user_type, _, _, _}) -> false;
is_not_user_type(_) -> true.

prop_normalize_type_(Type) ->
    %% Try to make these rules easily copy-pastable to the Erlang shell,
    %% so predefine args, use only exported functions, etc.
    Forms = [],
    ParseData = typechecker:collect_specs_types_opaques_and_functions(Forms),
    Opts = [],
    Env = typechecker:create_env(ParseData, Opts),
    typechecker:normalize(Type, Env),
    %% we're only interested in normalize termination / infinite recursion
    true.
