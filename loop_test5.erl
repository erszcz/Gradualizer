-module(even_more_recursive_types).

-compile([export_all, nowarn_export_all]).

-type rec1() :: rec1 | {rec1, rec1()} | {rec1, {rec1, rec1()}}.

-spec unwrap(rec1()) -> rec1().
unwrap({_, Z}) -> Z.
