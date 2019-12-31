%%%-------------------------------------------------------------------
%%% Created : 3 Jan 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc
%%% This is the KSZ8895 top level supervisor where the user can create
%%% or remove KSZ8895 gen-servers.
%%% @end
%%%-------------------------------------------------------------------

-module(ksz8895_sup).

-behaviour(supervisor).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1, create_ksz8895/1, remove_ksz8895/1]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(KSZ8895_DRIVER_NAME, ksz8895_driver).
-define(KSZ8895_MODULE_NAME, ksz8895).
-define(KSZ8895_TIMEOUT, 5000). % ms

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 4,
                 period => 30},
    %% Ksz8895 driver is linked at the initialisation
    ChildSpecs = [#{id => ?KSZ8895_DRIVER_NAME,
                    start => {?KSZ8895_DRIVER_NAME, start_link, []},
                    shutdown => brutal_kill}],
    % comment this line to stop trapping exits
    process_flag(trap_exit, true),
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

create_ksz8895(Instance) ->
  Ksz8895Name = compose_ksz8895_name(Instance),
  Ksz8895Spec = {Ksz8895Name, { ?KSZ8895_MODULE_NAME, start_link, [[Ksz8895Name, Instance]]},
        permanent, ?KSZ8895_TIMEOUT, worker, [?KSZ8895_MODULE_NAME]},
  supervisor:start_child(?MODULE, Ksz8895Spec).

remove_ksz8895(Instance) ->
  Ksz8895Name = compose_ksz8895_name(Instance),
  supervisor:terminate_child(?MODULE, Ksz8895Name),
  supervisor:delete_child(?MODULE, Ksz8895Name).

%%====================================================================
%% Internal functions
%%====================================================================

i2l(I) -> erlang:integer_to_list(I).
l2a(L) -> erlang:list_to_atom(L).

%% compose gen server name
compose_ksz8895_name(Id) ->
  Name = "Ksz8895:"++i2l(Id),
  l2a(Name).
