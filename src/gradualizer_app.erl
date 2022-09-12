%%%-------------------------------------------------------------------
%%% @doc Gradualizer application
%%%-------------------------------------------------------------------
-module(gradualizer_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1
        ]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->

    F = fun
            ({trace, _Pid, call, {_M, _F = do_type_check_expr_in, [_Env | _Args]}}, ok) ->
                Trace = {trace, _Pid, call, {_M, _F, [env | _Args]}},
                io:format("~p\n", [Trace]);
            ({trace, _Pid, return_from, {_M, _F = do_type_check_expr_in, _Arity}, _RetVal}, ok) ->
                Trace = {trace, _Pid, return_from, {_M, _F, _Arity}, {env, constraints}},
                io:format("~p\n", [Trace]);
            (Trace, ok) ->
                io:format("~p\n", [Trace])
        end,
    %dbg:tracer(process, {F, ok}),
    dbg:p(all, call),
    %dbg:tpl(typechecker, expect_tuple_type, x),
    %dbg:tpl(typechecker, compat, x),
    %dbg:tpl(typechecker, compat_seen, x),
    %dbg:tpl(typechecker, compat_ty, x),
    %dbg:tpl(typechecker, debug, x),
    %dbg:tpl(typechecker, do_type_check_expr_in, x),
    dbg:tpl(typechecker, expect_fun_type, x),

    Opts = application:get_env(gradualizer, options, []),
    gradualizer_sup:start_link(Opts).

stop(_State) ->
    ok.
