%% @private
%% @doc Stores original annotations of types for reporting.
-module(gradualizer_anno).

-export([start/0,
         record/2,
         lookup/1]).

start() ->
    ?MODULE = ets:new(?MODULE, [named_table, bag, public]),
    ok.

record(Key, Loc) ->
    ets:insert(?MODULE, {Key, Loc}).

lookup(Key) ->
    ets:lookup(?MODULE, Key).
