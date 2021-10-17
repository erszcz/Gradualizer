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
    dbg:tracer(process, {fun
                             (Trace, St) ->
                                 io:format("~p\n\n", [Trace]),
                                 St+1
                         end, 0}),
    %dbg:p(all, [call, arity]),
    %dbg:p(all, [call, return_to]),
    dbg:p(all, [call]),

    %dbg:tpl(typechecker, check_clauses, x),
    %dbg:tpl(typechecker, check_clause, x),
    %dbg:tpl(typechecker, add_type_pat, x),
    %dbg:tpl(typechecker, rewrite_list_to_nonempty_list, x),
    %dbg:tpl(typechecker, refine_vars_by_mismatching_clause, x),
    %dbg:tpl(typechecker, refine_clause_arg_tys, x),
    dbg:tpl(typechecker, type_diff, x),
    dbg:tpl(typechecker, refine, x),
    dbg:tpl(typechecker, refine_ty, x),
    %dbg:tpl(typechecker, expect_list_type, x),
    %dbg:tpl(typechecker, check_exhaustiveness, x),

    Opts = application:get_env(gradualizer, options, []),
    gradualizer_config:set_union_size_limit(Opts),
    gradualizer_sup:start_link(Opts).

stop(_State) ->
    ok.
