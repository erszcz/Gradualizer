-module(recursive_record).

-compile([export_all, nowarn_export_all]).

-type rec(A) :: A | #rec{rec :: A | rec(A)}.
-record(rec, {rec :: any()}).

-spec unwrap(rec(#rec{rec :: A})) -> rec(A).
unwrap(#rec{rec = Rec}) -> Rec.
