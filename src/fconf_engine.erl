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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {parser, store}).

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
start_link(Parser) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Parser], []).


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
init([Parser]) ->
    {ok, #state{parser=Parser, state=[]}}.

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
handle_call({get, Key}, _From, State) ->
    Reply = get_item(Key, State),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast({add, Key, Value}, State) ->
    add_item(Key, Value, State),
    {noreply, State}.

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
%% Internal Functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec accept_request() -> ok.
%% 
%% @doc 
%%  Simple init function for accept_request main loop.
%% @end
%% @private
%%--------------------------------------------------------------------
accept_request() ->
    accept_request([]).

%%-------------------------------------------------------------------
%% @spec accept_request(State) -> ok.
%%
%% @doc
%%   start accepting requests for configuration information
%% @end
%% @private
%%-------------------------------------------------------------------
accept_request(State) ->
    receive 

            accept_request(State);


            accept_request(NState);
        {delete, _Pid, Key} ->
            NState = delete_item(Key, State),
            accept_request(NState);
        {parse, Pid, BuildFile} ->
            NState = handle_parse_output(Pid, State, 
                                         parse_config_file(BuildFile)),
            accept_request(NState);
        exit ->
            ok;
        _ ->
            accept_request(State)
    end.


%%--------------------------------------------------------------------
%% @spec handle_parse_output(Pid, State, Ret) -> State | NewState.
%% 
%% @doc 
%%  Handle the output of parseconfig. If an error occures send
%%  a message to the calling process to let them know the problem
%% then continue.
%% @end
%% @private
%%--------------------------------------------------------------------
handle_parse_output(Pid, State, Ret = {error, _Error}) ->
    Pid ! Ret,
    State;
handle_parse_output(Pid, State, {NState, _, _}) when is_list(NState) ->
    Pid ! ok,
    State ++ NState.

%%--------------------------------------------------------------------
%% @spec parse_config_file(BuildFile) -> ParsedConfig.
%% 
%% @doc 
%%  Read in the correct config file. Root specifies server root and 
%%  env specifies the runtime environment.
%% @end
%% @private
%%--------------------------------------------------------------------
parse_config_file(BuildFile) ->
    case file:read_file(BuildFile) of
        {ok, FileBin} ->
            parse_config(binary_to_list(FileBin), 0, 0);
        Else ->
            Else
    end.


%%--------------------------------------------------------------------
%% @spec parse_config(Stream) -> ParsedConfig.
%% 
%% @doc 
%%  Parse the config  file into a usable format.
%% @end
%% @private
%%--------------------------------------------------------------------
parse_config([$\s | T], NewLines, Chars) ->
    parse_config(T, NewLines, Chars + 1);
parse_config([$\t | T], NewLines, Chars) ->
    parse_config(T, NewLines, Chars + 1);
parse_config([$\n | T], NewLines, _Chars) ->
    parse_config(T, NewLines + 1, 0);
parse_config([$\r | T], NewLines, _Chars) ->
    parse_config(T, NewLines + 1, 0);
parse_config(All = [${ | _], NewLines, Chars) ->
    case  ktuo_json:decode(All, NewLines, Chars) of 
        Error = {error, _} ->
            Error;
        Value ->
            Value
    end;
parse_config(All, NewLines, Chars) ->
   case ktuo_json:decode([${ | All] ++ [$}], NewLines, Chars) of
        Error = {error, _} ->
            Error;
        Value ->
            Value
   end.

%%--------------------------------------------------------------------
%% @spec tuplize(Key], TAcc, Acc) -> {path, PathList}.
%% 
%% @doc 
%%  Split the dot seperated path into a true path type.
%% @end
%% @private
%%--------------------------------------------------------------------
tuplize([$. | T], TAcc, Acc) ->
    tuplize(T, [], [lists:reverse(TAcc) | Acc]);
tuplize([H | T], TAcc, Acc) ->
    tuplize(T, [H | TAcc], Acc);
tuplize([], [], Acc) ->
    {path, lists:reverse(Acc)};
tuplize([], TAcc, Acc) ->
    {path, lists:reverse([lists:reverse(TAcc) | Acc])}.


%%--------------------------------------------------------------------
%% @spec get_item(Key, Config) -> Value.
%% 
%% @doc 
%%  Get the item from the system identified by the key path.
%% @end
%% @private
%%--------------------------------------------------------------------
get_item(_Key, undefined) ->
    undefined;
get_item([H | T], Config) ->
    get_item(T, get_val(H, Config));
get_item([], Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec get_val(Item, List) -> Value.
%% 
%% @doc 
%%  Get the specified item from the prop list.
%% @end
%% @private
%%--------------------------------------------------------------------
get_val(Item, [{Item, Value} | _]) ->
    Value;
get_val(Item, [ _ | T]) ->
    get_val(Item, T);
get_val(_Item, []) ->
    undefined.



%%--------------------------------------------------------------------
%% @spec mod_val(Item, Config, Acc) -> Value | {undefined, Acc}. 
%% 
%% @doc 
%%  Get a value along with context.
%% @end
%% @private
%%--------------------------------------------------------------------
mod_val(Item, [{Item, Value} | T], Acc) ->
    {Value, Acc, T};
mod_val(Item, [ H | T], Acc) ->
    mod_val(Item, T, [H | Acc]);
mod_val(_Item, [], Acc) ->
    {undefined, Acc}.

%%--------------------------------------------------------------------
%% @spec add_item(KeyList, Value, Config) -> value_mismatch | no_key |
%%   NewConfig.
%% 
%% @doc 
%%   Add an item into the system. 
%% @end
%% @private
%%--------------------------------------------------------------------
add_item([H], Value = [{_K, _V} | _], Config) ->
    case mod_val(H, Config, []) of
        {undefined, Acc}  ->
            [{H, Value} | Acc]; 
        {NValue = [{_Key, _Value} | _], Acc, Rest} ->
            [{H, Value ++ NValue} | Acc] ++ Rest
    end;
add_item([H], Value = {_K, _V}, Config) ->
    case mod_val(H, Config, []) of
        {undefined, Acc}  ->
            [{H, Value} | Acc]; 
        {NValue = [{_Key, _Value} | _], Acc, Rest} ->
            [{H, [Value | NValue]} | Acc] ++ Rest
    end;
add_item([H], Value, Config) ->
    case mod_val(H, Config, []) of
        {undefined, Acc}  ->
            [{H, Value} | Acc]; 
        {_NValue, _Acc, _Rest} ->
            value_mismatch
    end;
add_item([H | T], Value, Config) ->
    case mod_val(H, Config, []) of
        {undefined, Acc}  ->
            NConfig = add_item(T, Value, []),
            [{H, NConfig} | Acc]; 
        {NValue, Acc, Rest} when is_list(NValue) ->
            NVal = add_item(T, Value, NValue),
            [{H, NVal} | Acc] ++ Rest;
        _Else ->
            value_mismatch
    end;
add_item([], _Value, _Config) ->
    no_key.

%%--------------------------------------------------------------------
%% @spec delete_item(Key, Config) -> NewConfig | Error.
%%   
%% 
%% @doc 
%%   Delete the item specified by key from the system.
%% @end
%% @private
%%--------------------------------------------------------------------
delete_item([H], Config) ->
    case mod_val(H, Config, []) of
        {undefined, Acc}  ->
            Acc;
        {_, Acc, Rest} ->
            Acc ++ Rest
    end;
delete_item([H | T], Config) ->
    case mod_val(H, Config, []) of
        {undefined, Acc}  ->
            Acc;
        {NValue, Acc, Rest} when is_list(NValue) ->
            NVal = delete_item(T, NValue),
            [{H, NVal} | Acc] ++ Rest;
        _Else ->
            value_mismatch
    end;
delete_item([], _Config) ->
    no_key.

%%====================================================================
%%% Tests
%%====================================================================
tuplize_test() ->
    ?assertMatch({path, ["Hello", "Hola"]}, 
                 tuplize("Hello.Hola", [], [])),
    ?assertMatch({path, ["One", "Two"]},
                 tuplize("One.Two.", [], [])).

get_item_test() ->
    ?assertMatch(99, get_item(["Hello", "Port"],
                              [{"Boo", "Blah"},
                               {"Hello", [{"Pah", 100}, 
                                          {"Port", 99}]}])),
    ?assertMatch([{"Hello", "Goodbuy"}],
                 get_item(["Brody", "Brady", "Brah"],
                          [{"Boo", 100},
                           {"Brak", "Boo"},
                           {"Brody", [{"pooky", "pah"},
                                      {"Brady", [{"Brah", 
                                                  [{"Hello", 
                                                    "Goodbuy"}]}]}]}])).

add_item_test() ->
    Config = [{"Boo", 100},
              {"Brak", "Boo"},
              {"Brody", [{"pooky", "pah"},
                         {"Brady", [{"Brah", 
                                     [{"Hello", 
                                       "Goodbuy"}]}]}]}],
    NConfig = add_item(["Brody", "Baa", "Baat"],
                       99,
                       Config),
    ?assertMatch(99, get_item(["Brody", "Baa", "Baat"],
                              NConfig)),
    ?assertMatch("Boo", get_item(["Brak"], NConfig)),
    
    NConfig1 = add_item(["Brody", "Baa", "Baata"],
                        "Hello",
                        NConfig),
    ?assertMatch("Hello", get_item(["Brody", "Baa", "Baata"],
                              NConfig1)),
    ?assertMatch(99, get_item(["Brody", "Baa", "Baat"],
                              NConfig1)),
    NConfig2 = add_item(["Brody", "Brady", "Baata"],
                        "Hello",
                        NConfig1),
    ?assertMatch("Hello", get_item(["Brody", "Brady", "Baata"],
                                   NConfig2)),
    ?assertMatch("Goodbuy", get_item(["Brody", "Brady", "Brah", "Hello"],
                                   NConfig2)),
    NConfig3 = add_item(["Brody", "Brady", "Brah"],
                        {"noodle", 43},
                        NConfig2),
    ?assertMatch(43, get_item(["Brody", "Brady", "Brah", "noodle"],
                              NConfig3)),
    ?assertMatch("Goodbuy", get_item(["Brody", "Brady", "Brah", "Hello"],
                                     NConfig3)),
    NConfig4 = add_item(["Brody", "Brady", "Brah"],
                        [{"le", 43}, {"lo", 21}],
                        NConfig3),
    ?assertMatch(43, get_item(["Brody", "Brady", "Brah", "noodle"],
                              NConfig4)),
    ?assertMatch("Goodbuy", get_item(["Brody", "Brady", "Brah", "Hello"],
                                     NConfig4)),
    ?assertMatch(43, get_item(["Brody", "Brady", "Brah", "le"],
                                     NConfig4)),
    ?assertMatch(21, get_item(["Brody", "Brady", "Brah", "lo"],
                                     NConfig4)).


delete_item_test() ->
    Config = [{"Boo", 100},
              {"Brak", "Boo"},
              {"Brasto", [{"bo", 2},
                          {"bok", 33},
                          {"bodor", "hello"}]},
              {"Brody", [{"pooky", "pah"},
                         {"Brady", [{"Brah", 
                                     [{"Hello", 
                                       "Goodbuy"}]}]}]}],
    NConfig0 = delete_item(["Brody", "Brady", "Brah", "Hello"],
                          Config),
    NConfig = delete_item(["Brasto", "bok"],
                          NConfig0),

    ?assertMatch(undefined, get_item(["Brasto", "Bok"],
                              NConfig)),
    ?assertMatch(undefined, get_item(["Brody", "Brady", "Brah", "Hello"],
                              NConfig)),
    ?assertMatch("hello", get_item(["Brasto", "bodor"],
                              NConfig)),
    ?assertMatch(2, get_item(["Brasto", "bo"],
                              NConfig)),
    ?assertMatch("Boo", get_item(["Brak"], NConfig)),
    
    ?assertMatch(100, get_item(["Boo"],
                               NConfig)),
    ?assertMatch([], get_item(["Brody", "Brady", "Brah"],
                                   NConfig)).

