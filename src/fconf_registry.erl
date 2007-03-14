%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%%  Name registry for started processes. 
%%% 
%%% @end
%%% @copyright (C) 2007 Eric Merritt
%%% Created : 11 Mar 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(fconf_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, find_registered/1, register_config/2, 
        unregister_config/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {names}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%% 
%% @doc 
%% Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc 
%%  Find the pid registered by the name. 
%%
%% @spec find_registered(Name) -> Pid.
%% @end
%%--------------------------------------------------------------------
find_registered(Name) ->
    gen_server:call(?SERVER, {get, Name}).

%%--------------------------------------------------------------------
%% @doc 
%%  register a pid at the name. If the name already exists
%%  associate withthe new pid.
%% @spec register(Name, Pid) -> ok.
%% @end
%%--------------------------------------------------------------------
register_config(Name, Pid) ->
    gen_server:cast(?SERVER, {register, Name, Pid}).

%%--------------------------------------------------------------------
%% @doc 
%%  Remove a registered name/pid pair.
%% @spec unregister(Name) -> ok.
%% @end
%%--------------------------------------------------------------------
unregister_config(Name) ->
    gen_server:cast(?SERVER, {unregister, Name}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}.
%% 
%% @doc 
%% Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{names=dict:new()}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call({get, Name}, _From, State = #state{names=Names}) ->
    case catch dict:fetch(Name, Names) of
        {'EXIT', _} ->
            {reply, undefined, State};
        Reply ->
            {reply, Reply, State}
    end.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast({register, Name, Pid}, #state{names=Names}) ->
    {noreply, #state{names=dict:store(Name, Pid, Names)}};
handle_cast({unregister, Name}, #state{names=Names}) ->
    {noreply, #state{names=dict:erase(Name,Names)}}.
     

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}.
%% 
%% @doc 
%% Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void().
%% 
%% @doc 
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}.
%% 
%% @doc 
%% Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================
