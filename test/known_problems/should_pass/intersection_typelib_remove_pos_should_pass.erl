-module(intersection_typelib_remove_pos_should_pass).

-export([remove_pos/1]).

%% This example is based on typelib:remove_pos/1.
%%
%% typelib:remove_pos/1 is initially called with a type() argument,
%% but then it recurses over children of type() nodes.
%% However, not all of them are type() themselves, which means that
%% typelib:remove_pos() has to have an intersection spec if
%% we want to handle all the children by a single function.
%%
%% Now, we have some support for function intersection types,
%% so in general it should work.
%% See intersection_typelib_remove_pos_pass for a simpler variant which actually does.
%%
%% However, type checking intersection typed functions relies on clauses of the function refining
%% and exhausting argument types of a spec clause.
%% Only once all argument types of the spec clause are exhausted,
%% the algorithm proceeds to type check the next spec clause.
%%
%% typelib:remove_pos/1, though, uses complex pattern matching together with guards.
%% These are too complex for Gradualizer to reliably use for refinement of the argument type type().
%% type() is not refined throughout the subsequent steps of check_clauses_intersection(),
%% even though a number of typelib:remove_pos/1 clauses successfully type checks against type().
%% Ultimately, instead of type() being refined to none(), so that the algo could switch to the next
%% spec clause, we reach a function clause with a pattern which doesn't type check against type():
%%
%%   The pattern {type, _, any} on line 40 at column 12 doesn't have the type type()
%%
%% The type checker behaviour can be watched by printing or tracing
%% RefinedArgsTyss in typechecker:check_clauses_intersection().

-type af_constraint() :: gradualizer_type:af_constraint().
-type binary_op() :: gradualizer_type:af_binary_op(_).
-type gr_any_fun_args() :: gradualizer_type:gr_any_fun_args().
-type type() :: gradualizer_type:abstract_type().
-type unary_op() :: gradualizer_type:af_unary_op(_).

-spec remove_pos(gr_any_fun_args()) -> ok;
                (af_constraint()) -> ok;
                (type()) -> ok;
                (unary_op()) -> ok;
                (binary_op()) -> ok.
%% This handles gr_any_fun_args()
remove_pos({type, _, any}) -> ok;
%% This handles af_constraint()
remove_pos({type, _, constraint, _}) -> ok;
%% These handle type()
remove_pos({Type, _, _Value})
  when Type == atom; Type == integer; Type == char; Type == var -> ok;
remove_pos({type, _, Type, any}) when Type == tuple; Type == map -> ok;
remove_pos({type, _, Assoc, _Tys})
  when Assoc == map_field_exact;
       Assoc == map_field_assoc -> ok;
remove_pos({remote_type, _, _}) -> ok;
remove_pos({ann_type, _, _}) -> ok;
%% These handle unary_op() and binary_op()
remove_pos({op, _, _Op, _Type}) -> ok;
remove_pos({op, _, _Op, _Type1, _Type2}) -> ok.
