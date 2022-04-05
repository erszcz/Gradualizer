-module(list_exhaustiveness_checking_regressions).

-export([
         %f/2, g/1, h/1,
         i/1,
         j/1
        ]).

-spec i([atom()]) -> ok.
i([]) -> ok;
i([Cs]) -> ok;
i([C1, C2 | Cs]) -> ok.

-spec j([atom()]) -> ok.
j([]) -> ok;
j([C1, C2 | _]) -> ok;
j([Cs]) -> ok.
