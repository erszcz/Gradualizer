-module(gradualizer_type_gen).

-export([abstract_type/0, abstract_type/1,
         expr/0, expr/1]).

abstract_type() ->
    Opts = [{weight, {binop, 0}},
            {weight, {unop, 0}},
            {weight, {remote_type, 0}},
            {weight, {user_defined_type, 0}}],
    abstract_type(Opts).

abstract_type(Opts) ->
    State = gradualizer_erlang_abstract_code:options(Opts),
    gradualizer_erlang_abstract_code:abstract_type(State).

expr() ->
    Opts = [],
    expr(Opts).

expr(Opts) ->
    gradualizer_erlang_abstract_code:expr(Opts).
