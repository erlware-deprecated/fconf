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
%%%  Supports the configuration engine. Allows
%%%  applications to query aspects of the data.
%%% @end
%%% Created : 30 Oct 2006 by Eric Merritt <cyberlync@gmail.com>
%%%----------------------------------------------------------------------------
-module(fconf_engine).

-behaviour(gen_server).

-include("eunit.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

-record(state, {parser, store, name}).

%%====================================================================
%% API 
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%% Starts the server
%%
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%% @end 
%%--------------------------------------------------------------------
start_link(Name, Parser) ->
    gen_server:start_link(?MODULE, [Name, Parser], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}.
%% @end 
%%--------------------------------------------------------------------
init([Name, Parser]) ->
    fconf_registry:register_config(Name, self()),
    {ok, #state{parser=Parser, name=Name, store=dict:new()}}.

%%--------------------------------------------------------------------
%% @doc 
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}.
%% @end 
%%--------------------------------------------------------------------
handle_call({get, {path, Key}}, _From, State = #state{store=Store}) ->
    case get_item(Key, Store) of
        {ok, Reply} ->
            {reply, Reply, State};
        error ->
            {reply, undefined, State}
    end;
handle_call({parse, BuildFile}, _From, State = #state{store=Store,
                                                     parser=Parser}) ->
    NStore = handle_parse_output(Parser(BuildFile), Store),
    {reply, ok, State#state{store=NStore}}.

%%--------------------------------------------------------------------
%% @doc 
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%% 
%% @end 
%%--------------------------------------------------------------------
handle_cast({add, {path, Key}, Value}, State = #state{store=Store}) ->
    case store(Key, Store, Value) of
        error ->
            error_logger:warning_msg("Unable to add ~w", [Key]),
            {noreply, State#state{store=Store}};
        NStore ->
            {noreply, State#state{store=NStore}}
    end;
handle_cast({delete, {path, Key}}, State = #state{store=Store}) ->
    case delete(Key, Store) of
        error ->
            error_logger:warning_msg("Unable to delete ~w", [Key]),
            {noreply, State#state{store=Store}};
        NStore ->
            {noreply, State#state{store=NStore}}
    end;
handle_cast(exit, _) ->
    exit(normal).


%%--------------------------------------------------------------------
%% @doc 
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}.
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc 
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void().
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, #state{name=Name}) ->
    fconf_registry:unregister_config(Name),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}.
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%%  Handle the output of parseconfig. If an error occures send
%%  a message to the calling process to let them know the problem
%% then continue.
%% @spec handle_parse_output(Pid, State, Ret) -> State | NewState.
%% @end
%% @private
%%--------------------------------------------------------------------
handle_parse_output(Store, NewStore) ->
    dict:merge(fun merge_key/3, Store, NewStore).


%%-------------------------------------------------------------------- 
%% @doc 
%%  merge the values for key. 
%% @spec merge_key(Key, V1, V2) -> NVal.
%% @end
%% @private
%%--------------------------------------------------------------------
merge_key(_, Val1, Val2) ->
    case both_dicts(Val1, Val2) of
        true ->
            dict:merge(Val1, Val2);
        false ->
            Val2
    end.


%%--------------------------------------------------------------------
%% @doc 
%%  Test if both values are dicts.
%% @spec both_dicts(V1, V2) -> true | false.
%% @end
%% @private
%%--------------------------------------------------------------------
both_dicts({dict, _, _, _, _, _, _, _, _},
           {dict, _, _, _, _, _, _, _, _}) ->
    true;
both_dicts(_, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc 
%%  Get the item from the dict recursivly pulling each value.
%% @spec get_item(Name, Dict) -> NDict.
%% @end
%% @private
%%--------------------------------------------------------------------
get_item([H], Dict) ->
    dict:find(H, Dict);
get_item([H | T], Dict) ->
    case is_dict(Dict) of
        true ->
            case dict:find(H, Dict) of
                {ok, Value} ->
                    get_item(T, Value);
                Err ->
                    Err
            end;
        false ->
            error
    end;
get_item([], _Dict) ->
    error.


%%--------------------------------------------------------------------
%% @doc 
%%  Delete the value from the dict recursively.
%% @spec delete(Key, Dict) -> NDict.
%% @end
%% @private
%%--------------------------------------------------------------------
delete([H], Dict) ->
    case is_dict(Dict) of
        true ->
            dict:erase(H, Dict);
        false ->
            error
    end;
delete([H | T], Dict) ->
    case is_dict(Dict) of
        true ->
            case dict:find(H, Dict) of
                {ok, TValue} ->
                    case delete(T, TValue) of
                        error ->
                            error;
                        N ->
                            dict:store(H, N, Dict)
                    end;
                _ ->
                    error
            end;
        false ->
            error
    end;
delete([], _Dict) ->
    error.


%%--------------------------------------------------------------------
%% @doc 
%%  Store the value recursively into the dict and sub dicts.
%% @spec store(Key, Dict, Value) -> NDict
%% @end
%% @private
%%--------------------------------------------------------------------
store([H], Dict, Value) ->
    dict:store(H, Value, Dict);
store([H | T], Dict, Value) ->
    case is_dict(Dict) of
        true ->
            case dict:find(H, Dict) of
                {ok, TValue} ->
                    case store(T, TValue, Value) of
                        error ->
                            error;
                        N ->
                            dict:store(H, N, Dict)
                    end;
                _ ->
                    New = dict:new(),
                    case store(T, New, Value) of
                        error ->
                            error;
                        N ->
                            dict:store(H, N, Dict)
                    end
            end;
        false ->
            error
    end;
store([], _Dict, _Value) ->
    error.

%%--------------------------------------------------------------------
%% @doc 
%%  Check to see if the value is a dict.
%% @spec is_dict(Value) -> true | false.
%% @end
%% @private
%%--------------------------------------------------------------------
is_dict({dict, _, _, _, _, _, _, _, _}) ->
    true;
is_dict(_) ->
    false.

%%====================================================================
%%% Tests
%%====================================================================
