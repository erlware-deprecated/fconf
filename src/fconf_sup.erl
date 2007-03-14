%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%% Supports the fconf engine parse handling.
%%% @end
%%% @copyright (C) 2007
%%% Created : 13 Mar 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(fconf_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_config/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%% 
%% @doc 
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}.
%% 
%% @doc 
%%  Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([Parser]) ->
    AChild = {fconf_engine, {fconfig, ,start_link,[Parser]},
              permanent,2000,worker,[fconf_engine]},
    {ok,{{simple_one_for_all,0,1}, [AChild]}}.

%%-------------------------------------------------------------------- 
%% @doc 
%%  Start a new config with the specified config name.
%% 
%% @spec start_config(Name) -> ok. 
%% @end
%%--------------------------------------------------------------------
start_config(Name, Handler) ->
    supervisor:start_child(?SERVER, [Name, Handler]).

%%====================================================================
%% Internal functions
%%====================================================================
