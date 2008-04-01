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
%% @spec start_link(Name, Parser) -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Parser) ->
    gen_server:start_link(?MODULE, [Name, Parser], {obj, []}).


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
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name, Parser]) ->
    fconf_registry:register_config(Name, self()),
    {ok, #state{parser=Parser, name=Name, store={obj, []}}}.

%%--------------------------------------------------------------------
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
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
%%                                      {stop, Reason, State}
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
%%                                       {stop, Reason, State}
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
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{name=Name}) ->
    fconf_registry:unregister_config(Name),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
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
%% @spec handle_parse_output(Store, NewStore) -> NewStore2
%% @end
%% @private
%%--------------------------------------------------------------------
handle_parse_output(Store, NewStore) ->
    merge(NewStore, Store, []).


%%--------------------------------------------------------------------
%% @doc
%%  merge the values for key.
%% @spec merge_key(Key, V1, V2) -> NVal
%% @end
%% @private
%%--------------------------------------------------------------------
merge_tuple(Val1, Val2) ->
    case both_objects(Val1, Val2) of
        true ->
            merge(Val1, Val2, []);
        false ->
            Val2
    end.

merge([Tuple = {NewKey, NewValue} | RestNew], Store, Acc) ->
    case lists:keysearch(NewKey, 1, Store) of
        {value, {_, Value}} ->
            merge(RestNew, Store, [{NewKey, merge_tuple(NewValue, Value)} |
                                   Acc]);
        false  ->
            merge(RestNew, Store, [Tuple | Acc])
    end;
merge({obj, NewObject}, {obj, OldObject}, Acc) ->
    merge(NewObject, OldObject,  Acc);
merge([], _, Acc) ->
    {obj, Acc}.

%%--------------------------------------------------------------------
%% @doc
%%  Test if both values are dicts.
%%
%% @spec (V1, V2) -> true | false
%% @end
%% @private
%%--------------------------------------------------------------------
both_objects({obj, _}, {obj, _}) ->
    true;
both_objects(_, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%%  Get the item from the dict recursivly pulling each value.
%% @spec (Name, Object) -> {ok, Item} | error
%% @end
%% @private
%%--------------------------------------------------------------------
get_item([H], {obj, Object}) ->
    case lists:keysearch(H, 1, Object) of
        {value, {H, Value}} ->
            {ok, Value};
        false ->
            error
    end;
get_item([H | T], {obj, Object}) ->
    case lists:keysearch(H, 1, Object) of
        {value, {H, Value}} ->
            get_item(T, Value);
        false ->
            error
    end;
get_item([], _) ->
    error.


%%--------------------------------------------------------------------
%% @doc
%%  Delete the value from the dict recursively.
%% @spec (Key, Object) -> NewObject | Error
%% @end
%% @private
%%--------------------------------------------------------------------
delete([H], {obj, Value}) ->
    {obj, lists:keydelete(H, 1, Value)};
delete([H | T], {obj, Value}) ->
    case lists:keysearch(H, 1, Value) of
        {value, {H, TValue}} ->
            case delete(T, TValue) of
                error ->
                    error;
                N ->
                    {obj, lists:keyreplace(H, 1, Value, {H, N})}
            end;
        false ->
            error
    end;
delete([], _) ->
    error.


%%--------------------------------------------------------------------
%% @doc
%%  Store the value recursively into the dict and sub dicts.
%% @spec (Key, Dict, Value) -> NDict
%% @end
%% @private
%%--------------------------------------------------------------------
store([H], {obj, Object}, Value) ->
   case lists:keytake(H, 1, Object) of
       {value, _, Object2} ->
           {obj, [{H, Value} | Object2]};
       false ->
           {obj, [{H, Value} | Object]}
   end;
store([H | T], {obj, Object}, Value) ->
    case lists:keysearch(H, 1, Object) of
        {value, {H,  TValue = {obj, _}}} ->
            case store(T, TValue, Value) of
                error ->
                    error;
                N ->
                    {obj, lists:keyreplace(H, 1, Object, {H, N})}
            end;
        {value, {H, _}} ->
            case store(T, {obj, []}, Value) of
                error ->
                    error;
                N ->
                    {obj, lists:keyreplace(H, 1, Object, {H, N})}
            end;
        false ->
            case store(T, {obj, []}, Value) of
                error ->
                    error;
                N ->
                    {obj, [{H, N} | Object]}
            end
    end;
store([], _, _) ->
    error.


%%====================================================================
%%% Tests
%%====================================================================
store_test() ->
    Store1 = store([<<"hello">>], {obj, []}, <<"super">>),
    ?assertMatch({ok, <<"super">>}, get_item([<<"hello">>], Store1)),
    Store2 = store([<<"hello">>, <<"goodbye">>], Store1, 33),
    Store3 = store([<<"hello">>, <<"foo">>], Store2, [1, 2, 3]),
    Store4 = store([<<"hello">>, <<"doob">>, <<"zoob">>], Store3, <<"HelloAll">>),
    ?assertMatch({ok, 33}, get_item([<<"hello">>, <<"goodbye">>], Store4)),
    ?assertMatch({ok, [1, 2, 3]}, get_item([<<"hello">>, <<"foo">>], Store4)),
    ?assertMatch({ok, <<"HelloAll">>}, get_item([<<"hello">>, <<"doob">>, <<"zoob">>], Store4)).


delete_test() ->
    Store1 = store([<<"hello">>], {obj, []}, <<"super">>),
    Store2 = store([<<"hello">>, <<"goodbye">>], Store1, 33),
    Store3 = store([<<"hello">>, <<"foo">>], Store2, [1, 2, 3]),
    Store4 = store([<<"hello">>, <<"doob">>, <<"zoob">>], Store3, <<"HelloAll">>),
    Store5 = delete([<<"hello">>, <<"foo">>], Store4),
    ?assertMatch(error, get_item([<<"hello">>, <<"foo">>], Store5)),
    Store6 = delete([<<"hello">>, <<"doob">>], Store5),
    ?assertMatch(error, get_item([<<"hello">>, <<"doob">>, <<"zoob">>], Store6)).

