%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Eric Merritt
%%%
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc
%%%  The config level supervisor.
%%% @end
%%% @copyright (C) 2007, Eric Merritt
%%% Created : 13 Mar 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(fconf_conf_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_config/2, stop_config/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Conf = {fconf_engine, {fconf_engine, start_link,[]},
            temporary,2000,worker,[fconf_engine]},
    {ok,{{simple_one_for_one,0,1}, [Conf]}}.

%%--------------------------------------------------------------------
%% @doc
%%  Start a new config with the specified config name,
%%  a parse handler and an override.
%%
%% @spec start_config(Name, Override) -> ok
%% @end
%%--------------------------------------------------------------------
start_config(Name, Override) ->
    supervisor:start_child(?SERVER, [Name, Override]).

%%--------------------------------------------------------------------
%% @doc
%%  Shut down a config entry.
%% @spec stop_config(Name) -> ok
%% @end
%%--------------------------------------------------------------------
stop_config(Name) ->
    supervisor:terminate_child(?SERVER, Name).

%%====================================================================
%% Internal functions
%%====================================================================
