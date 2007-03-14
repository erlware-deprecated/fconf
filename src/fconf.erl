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


-export([start_config/2, parse_config/1, add/2, get_value/1, 
         get_value/2, delete/1, exit/0]).

%%====================================================================
%% API 
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%%   Start a new config 
%% @spec parse_config(BuildFile) -> ok.
%% @end
%%--------------------------------------------------------------------
start_config(Name, Handler) ->
    fconf_sup:start_config(Name, Handler).


%%--------------------------------------------------------------------
%% @spec parse_config(BuildFile) -> ok.
%% 
%% @doc 
%%  Parse the buildfile specified and merge it at the top level.
%% @end
%%--------------------------------------------------------------------
parse_config(BuildFile) ->
    gen_server:call(fconf_main, {parse, self(), BuildFile}, 5000).


%%-------------------------------------------------------------------
%% @spec add(Key::atom(), Value::any()) -> ok.
%%
%% @doc
%%  Add a key to the config.
%% @dec
%%-------------------------------------------------------------------
add({path, Path}, Value) ->
    gen_server:cast(fconf_main, {add, Path, Value},
    ok;
add(Key, Value) when is_list(Key)->
    add(tuplize(Key, [], []), Value).


%%-------------------------------------------------------------------
%% @spec get(Key::atom()) -> Value | undefined.
%%
%% @doc
%%  Get a value from the config.
%% @dec
%%-------------------------------------------------------------------
get_value({path, Path}) ->
    gen_server:call(fconf_main, {path, Path}).
get_value(Key) ->
    get_value(tuplize(Key, [], [])).

%%--------------------------------------------------------------------
%% @spec get(Key, Default) -> Value | Default.
%% 
%% @doc 
%%  Attempts to get the specified key. If the key doesn't exist it
%%  returns the requested default instead of just undefined.
%% @end
%%--------------------------------------------------------------------
get_value(Key, Default) ->
    case get_value(Key) of
        undefined ->
            Default;
        Else ->
            Else
    end.

%%-------------------------------------------------------------------
%% @spec delete(Key) -> ok.
%%
%% @doc
%%  Delete a value from the config.
%% @dec
%%-------------------------------------------------------------------
delete({path, Key}) ->
    gen_server:cast(fconf_main, {delete,  Key};
delete(Key) when is_list(Key) ->
    delete(tuplize(Key, [], [])).

%%--------------------------------------------------------------------
%% @spec exit() -> ok.
%% 
%% @doc 
%%  Tell sin_config to shutdown.
%% @end
%%--------------------------------------------------------------------
exit() ->
    gen_server:cast(fconf_main, exit).
%%====================================================================
%% Internal Functions
%%====================================================================
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


