-module(gradualizer_prop).

-compile([export_all, nowarn_export_all]).

-include_lib("proper/include/proper.hrl").

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
