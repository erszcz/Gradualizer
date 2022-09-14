-module(pattern_match_remote_complex_type_pass).

-type type() :: gradualizer_test_type:abstract_type().

-spec normalize_rec(type()) -> type().
normalize_rec({type, _, union, _Tys} = Type) ->
    Type.
