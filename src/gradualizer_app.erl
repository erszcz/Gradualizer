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
            (Trace, ok) ->
                io:format("~p\n", [Trace])
        end,
    dbg:tracer(process, {F, ok}),
    dbg:p(all, call),
    %dbg:tpl(typechecker, expect_map_type, x),
    %dbg:tpl(typechecker, add_type_pat, x),
    %dbg:tpl(typechecker, do_type_check_expr, x),
    %dbg:tpl(typechecker, type_check_expr_in, x),
    %dbg:tpl(typechecker, type_check_call_ty, x),
    dbg:tpl(typechecker, debug1, x),

    Opts = application:get_env(gradualizer, options, []),
    gradualizer_sup:start_link(Opts).

stop(_State) ->
    ok.
