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
%%%  Supports configuration aspects of the build system. Allows
%%%  applications to query aspects of the data.
%%% @end
%%% Created : 10 March 2007 Eric Merritt <cyberlync@gmail.com>
%%%----------------------------------------------------------------------------
-module(fconf).


-export([start_config/2, stop_config/1, add_config/2, store/3, get_value/2,
         get_value/3, delete/2, exit/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%   Start a new config with a handler and and override
%%
%% @spec start_config(Name, Override) -> ok
%% @end
%%--------------------------------------------------------------------
start_config(Name, Override) ->
    fconf_conf_sup:start_config(Name, Override).

%%--------------------------------------------------------------------
%% @doc
%%  Stop the config identified by the name.
%% @spec stop_config(Name) -> ok
%% @end
%%--------------------------------------------------------------------
stop_config(Name) ->
    fconf_conf_sup:stop_config(Name).

%%--------------------------------------------------------------------
%% @doc
%%  Add a config to the system. The config must be in the format
%% that the system can understand and use.
%%
%% @spec add_config(Name, AdditionalConfig) -> ok
%% @end
%%--------------------------------------------------------------------
add_config(Name, AdditionalConfig) ->
    Pid = fconf_registry:find_registered(Name),
    gen_server:call(Pid, {merge, AdditionalConfig}, 5000).


%%-------------------------------------------------------------------
%% @doc
%%  Add a key to the config.
%%
%% @spec store(Name, Path, Value::any()) -> ok
%% @end
%%-------------------------------------------------------------------
store(Name, Path = {path, _}, Value) ->
    Pid = fconf_registry:find_registered(Name),
    gen_server:cast(Pid, {add, Path, Value}),
    ok;
store(Name, Key, Value) when is_list(Key)->
    store(Name, tuplize(Key, [], []), Value).


%%-------------------------------------------------------------------
%% @doc
%%  Get a value from the config.
%%
%% @spec get_value(Name, Path) -> Value | undefined
%% @end
%%-------------------------------------------------------------------
get_value(Name, Path = {path, _}) ->
    Pid = fconf_registry:find_registered(Name),
    gen_server:call(Pid, {get, Path});
get_value(Name, Key) ->
    get_value(Name, tuplize(Key, [], [])).

%%--------------------------------------------------------------------
%% @doc
%%  Attempts to get the specified key. If the key doesn't exist it
%%  returns the requested default instead of just undefined.
%%
%% @spec get_value(Name, Key, Default) -> Value | Default
%% @end
%%--------------------------------------------------------------------
get_value(Name, Key, Default) ->
    case get_value(Name, Key) of
        undefined ->
            Default;
        Else ->
            Else
    end.

%%-------------------------------------------------------------------
%% @doc
%%  Delete a value from the config.
%%
%% @spec delete(Name, Key) -> ok
%% @end
%%-------------------------------------------------------------------
delete(Name, Key = {path, _}) ->
    Pid = fconf_registry:find_registered(Name),
    gen_server:cast(Pid, {delete,  Key});
delete(Name, Key) when is_list(Key) ->
    delete(Name, tuplize(Key, [], [])).

%%--------------------------------------------------------------------
%% @doc
%%  Tell sin_config to shutdown.
%%
%% @spec exit(Name) -> ok
%% @end
%%--------------------------------------------------------------------
exit(Name) ->
    Pid = fconf_registry:find_registered(Name),
    gen_server:cast(Pid, exit).

%%====================================================================
%% Internal Functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Split the dot seperated path into a true path type.
%%
%% @spec tuplize(Key, TAcc, Acc) -> {path, PathList}
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


