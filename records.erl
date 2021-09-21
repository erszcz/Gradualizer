-module(records).

-record(variant1, {a}).
-record(variant2, {b}).

-spec f(#variant1{a :: boolean()}) -> #variant1{a :: integer()}.
f(#variant1{} = Ctx) ->
    Ctx#variant1{a = 5}.


-type record_sum_t() :: #variant1{}
                      | #variant2{}.

-spec record_variants(record_sum_t()) -> {ok, any()}.
record_variants(T) ->
    case T of
        #variant1{a = A} -> {ok, A}
    end.
