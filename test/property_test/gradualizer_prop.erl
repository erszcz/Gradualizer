-module(gradualizer_prop).

-compile([export_all]).

-include_lib("proper/include/proper.hrl").

-type t() :: {atom, atom()} | {integer, integer()}.

anno() ->
    %% TODO: add missing file info
    ?LET({Line, Col}, {non_neg_integer(), pos_integer()}, {Line, Col}).

abstract_type() ->
    %% TODO: add the rest of abstract_type variants
    oneof([
           ?LET(T, af_atom(), T)
          ]).

af_atom() ->
    ?LET({Atom, Anno}, {range($0, $~), anno()}, {'atom', Anno, list_to_atom([Atom])}).

prop_remove_pos_removes_pos() ->
    ?FORALL(Type, abstract_type(),
            is_pos_removed(typelib:remove_pos(Type))).

is_pos_removed({atom, 0, _}) -> true;
is_pos_removed(_) -> false.

prop_t_is_an_atom_or_an_integer() ->
    ?FORALL(Type, t(),
            is_atom_or_integer(Type)).

is_atom_or_integer({atom, At}) when is_atom(At) -> true;
is_atom_or_integer({integer, Int}) when is_integer(Int) -> true;
is_atom_or_integer(_) -> false.

prop_normalize_type() ->
    ?FORALL(Type, gradualizer_type:abstract_type(),
            begin
                %% Try to make these rules easily copy-pastable to the Erlang shell,
                %% so predefine args, use only exported functions, etc.
                ct:pal("~s type:\n~p\n", [?FUNCTION_NAME, Type]),
                Forms = [],
                ParseData = typechecker:collect_specs_types_opaques_and_functions(Forms),
                Opts = [],
                Env = typechecker:create_env(ParseData, Opts),
                typechecker:normalize(Type, Env),
                true %% we're only interested in normalize termination / infinite recursion
            end).
