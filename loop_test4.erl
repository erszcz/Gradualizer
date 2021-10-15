-module(more_recursive_types).

-compile([export_all, nowarn_export_all]).

-type rec1(A) :: A | rec1({A | rec1(A)}).

-spec unwrap(rec1(A)) -> atom().
unwrap({rec, Z}) -> Z.
