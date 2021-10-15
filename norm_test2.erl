-module(norm_test2).

-compile(export_all).

-type apa(A) :: A | apa({A}).

-spec f(apa(integer())) -> ok.
f(_) -> ok.
