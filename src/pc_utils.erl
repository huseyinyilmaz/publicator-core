%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 06 Jul 2013 by Huseyin Yilmaz <huseyin@lhuseyin-work>
%%%-------------------------------------------------------------------
-module(pc_utils).

%% API
-export([generate_code/0, get_env/3, set_env/3, ensure_started/1]).
-export([get_channel_config/1]).
-export([make_message/5]).
-export([get_by_attribute/3]).

-include("../include/publicator_core.hrl").

-define(DEFAULT_CACHE_SIZE, 20).
-define(DEFAULT_TIMEOUT, 10 * 60 * 1000).       %10 minutes

-define(DEFAULT_CHANNELS, [[{name, all},
                            {cache_size, ?DEFAULT_CACHE_SIZE},
                            {timeout, ?DEFAULT_TIMEOUT}]]).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generates random code as binary
%% @end
%%--------------------------------------------------------------------
-spec generate_code() -> binary().
generate_code()->
    list_to_binary(
      string:to_lower(
	integer_to_list(
	  erlang:phash2({node(), erlang:timestamp()}), 36))).

%%--------------------------------------------------------------------
%% @doc
%% gets configuration for given app from sys.config file.
%% if configuration does not exists, it uses given default value.
%% @end
%%--------------------------------------------------------------------
-spec get_env(atom(), atom(), any()) -> any().
get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
	undefined -> Default;
	{ok, Value} -> Value
    end.

%%--------------------------------------------------------------------
%% @doc
%% Set environment variable for given app
%% @end
%%--------------------------------------------------------------------
-spec set_env(atom(), atom(), any()) -> any().
set_env(AppName, Key, Value) ->
    application:set_env(AppName, Key, Value).


-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

get_channel_config(Channel_code)->
    Channel_config_list = get_env(server, channels, ?DEFAULT_CHANNELS),
    Config = choose_channel_config(Channel_code, Channel_config_list),
    Config_timeout = proplists:get_value(timeout, Config, ?DEFAULT_TIMEOUT),
    lager:debug("Choosen configuration for channel ~p is ~p", [Channel_code, Config]),
    {Config_timeout}.


-spec make_message(code(), code(), message_type(), binary(), message_meta()) -> #message{}.
make_message(Producer_code, Channel_code, Type, Data, Meta) when is_atom(Type)->
        #message{
           producer_code=Producer_code,
           channel_code=Channel_code,
           type=Type,
           data=Data,
           meta=Meta}.

-spec get_by_attribute(atom(), binary(), []) -> [].
get_by_attribute(Attr, Code, [Arg| Args]) ->
    case proplists:get_value(Attr, Arg, all) of
        Code -> Arg;
        all -> Arg;
        _ ->
            get_by_attribute(Attr, Code, Args)
    end.
    

%%%===================================================================
%%% Internal functions
%%%===================================================================


choose_channel_config(Channel_code,[Config|Channel_config_list]) ->
    Config_name = proplists:get_value(name, Config, all),
    case Config_name of
        Channel_code -> Config;
        all -> Config;
        _ ->
            choose_channel_config(Channel_code, Channel_config_list)
    end.
            

