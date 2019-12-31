%%%-------------------------------------------------------------------
%% @doc ksz8895 public API
%% @end
%%%-------------------------------------------------------------------

-module(ksz8895_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ksz8895_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
