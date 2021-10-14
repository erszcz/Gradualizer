-module(norm_test).
-compile(export_all).
%-export([format_location/2, format_type_error/2, print_errors/2, handle_type_error/2]).

-spec describe_expr(gradualizer_type:abstract_expr()) -> ok.
describe_expr({call, _, _, _})            -> ok.

%describe_expr({atom, _, _})               -> ok.
%describe_expr({bc, _, _, _})              -> ok;
%describe_expr({bin, _, _})                -> ok;
%describe_expr({block,_,_})                -> ok;
%describe_expr({char, _, _})               -> ok;
%describe_expr({'catch', _, _})            -> ok;
%describe_expr({'case', _, _, _})          -> ok;
%describe_expr({cons, _, _, _})            -> ok;
%describe_expr({float, _, _})              -> ok;
%describe_expr({'fun', _, _})              -> ok;
%describe_expr({integer, _, _})            -> ok;
%describe_expr({'if', _, _})               -> ok;
%describe_expr({lc, _, _, _})              -> ok;
%describe_expr({map, _, _})                -> ok;
%describe_expr({map, _, _, _})             -> ok;
%describe_expr({match, _, _, _})           -> ok;
%describe_expr({named_fun, _, _, _})       -> ok;
%describe_expr({nil, _})                   -> ok;
%describe_expr({op, _, 'not', _})          -> ok;
%describe_expr({op, _, '-', _})            -> ok;
%describe_expr({op, _, Op, _, _})          -> ok;
%describe_expr({record, _, _, _})          -> ok;
%describe_expr({'receive', _, _, _, _})    -> ok;
%describe_expr({record, _, _, _, _})       -> ok;
%describe_expr({record_field, _, _, _, _}) -> ok;
%describe_expr({record_index, _, _, _})    -> ok;
%describe_expr({string, _, _})             -> ok;
%describe_expr({tuple, _, _})              -> ok;
%describe_expr({'try', _, _, _, _, _})     -> ok;
%describe_expr({var, _, _})                -> ok;
%describe_expr(_)                          -> ok.
