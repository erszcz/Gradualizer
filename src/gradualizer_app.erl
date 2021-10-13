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
                             %({trace, Pid, call, {M, handle_call, [Msg, From, _State]}}, ok) ->
                             %    Trace = {trace, Pid, call, {M, handle_call, [Msg, From, '#state{}']}},
                             %    io:format("~p\n\n", [Trace]);
                             %({trace, Pid, call, {M, Fun, [Msg, _State]}}, ok) when
                             %      Fun =:= handle_cast;
                             %      Fun =:= handle_info ->
                             %    Trace = {trace, Pid, call, {M, Fun, [Msg, '#state{}']}},
                             %    io:format("~p\n\n", [Trace]);
                             %({trace, Pid, return_from, {M, Fun, Arity}, {ok, Type}}, ok) ->
                             %    TypeS = typelib:pp_type(Type),
                             %    Trace = {trace, Pid, return_from, {M, Fun, Arity}, 'Type'},
                             %    io:format("~p\nType = ~ts\n\n", [Trace, TypeS]);
                             %(Trace, ok) ->
                             %    io:format("~p\n\n", [Trace])
                             (_Trace, ok) ->
                                 io:format(".", [])
                         end, ok}),
    %dbg:p(all, [call, arity]),
    %dbg:p(all, [call, return_to]),
    dbg:p(all, [call]),
    %dbg:tpl(typechecker, []),
    %dbg:tpl(typechecker, refinable, 3, x),
    %dbg:tpl(typechecker, type_diff, x),
    %dbg:tpl(typelib, remove_pos, x),
    dbg:tpl(typechecker, normalize, []),
    %dbg:tpl(typechecker, refine_clause_arg_tys, x),
    %dbg:tpl(typechecker, add_type_pat, x),
    %dbg:tpl(gradualizer_lib, get_type_definition, x),
    %dbg:tpl(typechecker, get_record_fields, x),
    %dbg:tpl(typechecker, get_maybe_remote_record_fields, x),
    %dbg:tpl(gradualizer_lib, get_type_definition, x),
    %dbg:tpl(gradualizer_db, start_link, []),
    %dbg:tpl(gradualizer_db, init, []),
    %dbg:tpl(gradualizer_db, import_prelude, []),
    %dbg:tpl(gradualizer_db, import_extra_specs, []),
    %dbg:tpl(gradualizer_db, handle_call, []),
    %dbg:tpl(gradualizer_db, handle_cast, []),
    %dbg:tpl(gradualizer_db, handle_info, []),
    %dbg:tpl(gradualizer_db, get_spec, x),
    %dbg:tpl(gradualizer_db, get_type, x),
    %dbg:tpl(typechecker, check_exhaustiveness, x),

    Opts = application:get_env(gradualizer, options, []),
    gradualizer_config:set_union_size_limit(Opts),
    gradualizer_sup:start_link(Opts).

stop(_State) ->
    ok.
