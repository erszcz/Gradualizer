-module(record_exhaustive).

-compile(export_all).

-record(inner, {
    field :: integer()
}).

-record(record_one_field, {
    inner_rec :: #inner{} | undefined
}).

-record(record_two_fields, {
    a :: #record_one_field{},
    b :: integer() | undefined
}).

-spec one_field(#record_one_field{} | undefined) -> integer().
one_field(#record_one_field{inner_rec = InnerRec = #inner{}}) -> InnerRec#inner.field;
%% unhandled
%%one_field(#record_one_field{field = undefined}) -> -1;
one_field(undefined) -> 0.

-spec two_fields(#record_two_fields{} | undefined) -> integer().
two_fields(#record_two_fields{a = #record_one_field{inner_rec = InnerRec = #inner{}}}) -> InnerRec#inner.field;
%% unhandled
%%two_fields(#record_two_fields{a = #record_one_field{inner_rec = undefined}}) -> 2;
two_fields(#record_two_fields{b = undefined}) -> 0;
%% unhandled
%%two_fields(#record_two_fields{b = B}) when is_integer(B) -> B;
two_fields(undefined) -> 0.

%% -spec test5(#record_two_fields{} | undefined) -> integer().
%% test5(#record_two_fields{a = #record_one_field{field = Field = #inner{}}}) -> Field#inner.field;
%% %% unhandled
%% %%test5(#record_two_fields{a = #record_one_field{field = undefined}}) -> 2;
%% test5(#record_two_fields{b = B}) when is_integer(B) -> B;
%% %% unhandled
%% %%test5(#record_two_fields{b = undefined}) -> 1;
%% test5(undefined) -> 0.


-record(union_rec, {
    field :: a | b | c
}).
-spec union_rec(#union_rec{}) -> integer().
union_rec(#union_rec{field = a}) ->
    0;
union_rec(#union_rec{field = b}) ->
    1.
%% unhandled
%% union_rec(#union_rec{field = c}) ->
%%     2.
