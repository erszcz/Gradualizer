-module(list_exhaustiveness_checking_regressions).

%-export([f/2, g/1, h/1, i/1, j/1, k/1]).
-compile(export_all).

f( Needle, [Needle | _]) -> ok;
f( Needle, [_ | Haystack]) -> f(Needle, Haystack);
f(_Needle, []) -> ok.

-spec g([a | b]) -> ok.
g([a | _Haystack]) -> ok;
g([_ | Haystack]) -> g(Haystack);
g([]) -> ok.

-spec h([a | b]) -> ok.
h([a | _Haystack]) -> ok;
h([]) -> ok;
h([_ | Haystack]) -> h(Haystack).

-spec i([atom()]) -> ok.
i([]) -> ok;
%i([_|_]) -> ok.
i([Cs]) -> ok;
i([C1, C2 | Cs]) -> ok.

-spec j([atom()]) -> integer().
j([]) -> 1;
j([C1, C2 | _]) -> 2;
j([Cs]) -> 3.

-spec k(list(atom())) -> integer().
k([_, _ | _]) -> 1;
k(_) -> 2.
