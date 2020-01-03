-module(guard).

-compile(export_all).

-spec atom(term()) -> atom() | not_atom.
atom(A) when is_atom(A) -> A;
atom(_) -> not_atom.

-spec binary(term()) -> binary() | not_binary.
binary(B) when is_binary(B) -> B;
binary(_) -> not_binary.

-spec bitstring(term()) -> bitstring() | not_bitstring.
bitstring(B) when is_bitstring(B) -> B;
bitstring(_) -> not_bitstring.

-spec boolean(term()) -> boolean() | not_boolean.
boolean(B) when is_boolean(B) -> B;
boolean(_) -> not_boolean.

-spec float(term()) -> float() | not_float.
float(F) when is_float(F) -> F;
float(_) -> not_float.

-spec integer(term()) -> integer() | not_integer.
integer(I) when is_integer(I) -> I;
integer(_) -> not_integer.

-spec number(term()) -> number() | not_number.
number(N) when is_number(N) -> N;
number(_) -> not_number.

-spec list(term()) -> list() | not_list.
list(L) when is_list(L) -> L;
list(_) -> not_list.

-spec map(term()) -> map() | not_map.
map(M) when is_map(M) -> M;
map(_) -> not_map.

-spec pid(term()) -> pid() | not_pid.
pid(P) when is_pid(P) -> P;
pid(_) -> not_pid.

-spec port(term()) -> port() | not_port.
port(P) when is_port(P) -> P;
port(_) -> not_port.

-spec reference(term()) -> reference() | not_reference.
reference(R) when is_reference(R) -> R;
reference(_) -> not_reference.

-spec tuple(term()) -> tuple() | not_tuple.
tuple(T) when is_tuple(T) -> T;
tuple(_) -> not_tuple.

-spec function(term()) -> function() | not_function.
function(F) when is_function(F) -> F;
function(_) -> not_function.

-record(r, {
    f
}).

-spec record(term()) -> #r{} | not_r.
record(R) when is_record(R, r) -> R;
record(_) -> not_r.

-spec multiple(term()) -> integer() | float() | not_number.
multiple(I) when is_integer(I) -> I;
multiple(F) when is_float(F) -> F;
multiple(_) -> not_number.

-spec orelse1(term()) -> integer() | float() | not_number.
orelse1(N) when is_integer(N) orelse is_float(N) -> N;
orelse1(_) -> not_number.

-spec orelse2(term()) -> integer() | float() | not_number.
orelse2(N) when is_integer(N); is_float(N) -> N;
orelse2(_) -> not_number.

-spec andalso1(term()) -> integer() | not_integer.
andalso1(N) when is_integer(N) andalso is_number(N) -> N;
andalso1(_) -> not_integer.

-spec andalso2(term()) -> integer() | not_integer.
andalso2(N) when is_integer(N), is_number(N) -> N;
andalso2(_) -> not_integer.