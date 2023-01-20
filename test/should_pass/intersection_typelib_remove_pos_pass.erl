-module(intersection_typelib_remove_pos_pass).

-export([remove_pos/1]).

%% See intersection_typelib_remove_pos_should_pass for the full problem description.

-type af_constraint() :: gradualizer_type:af_constraint().
-type binary_op() :: gradualizer_type:af_binary_op(_).
-type gr_any_fun_args() :: gradualizer_type:gr_any_fun_args().
-type type() :: gradualizer_type:abstract_type().
-type unary_op() :: gradualizer_type:af_unary_op(_).

-spec remove_pos(gr_any_fun_args()) -> ok;
                (af_constraint()) -> ok;
                %(type()) -> atom().
                (unary_op()) -> ok;
                (binary_op()) -> ok.
remove_pos({type, _, any}) -> ok;
remove_pos({type, _, constraint, _}) -> ok;
%remove_pos({Type, _, Value})
%  when Type == atom; Type == integer; Type == char; Type == var -> ok1;
%remove_pos({user_type, Anno, Name, Params}) -> ok2;
%remove_pos({type, Anno, record, _}) -> ok3;
%remove_pos({type, _, field_type, _}) -> ok4;
%remove_pos({type, _, Type, Params}) -> ok5;
%remove_pos({type, _, Type, any}) when Type == tuple; Type == map -> ok6;
%remove_pos({type, _, Assoc, Tys})
%  when Assoc == map_field_exact;
%       Assoc == map_field_assoc -> ok7;
%remove_pos({remote_type, _, _}) -> ok8;
%remove_pos({ann_type, _, _}) -> ok9.
remove_pos({op, _, Op, Type}) -> ok;
remove_pos({op, _, Op, Type1, Type2}) -> ok.
