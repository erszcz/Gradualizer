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
    %dbg:tpl(typechecker, add_type_pat, x),
    dbg:tpl(typechecker, set_var_type, x),
    dbg:tpl(typechecker, update_var_type, x),

    Opts = application:get_env(gradualizer, options, []),
    gradualizer_sup:start_link(Opts).

stop(_State) ->
    ok.
