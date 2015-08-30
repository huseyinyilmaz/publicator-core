%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>

-module(publicator_core).


%% API
-export([start/0, stop/0]).
-export([get_messages/1, publish/1,
	 subscribe/3, unsubscribe/2,
	 get_subscribtions/1,
	 create_producer/1, get_producer/1, get_channels/0]).
-export([stop_producer/1]).
-export([get_producers/3]).
-export([add_message_handler/2, remove_message_handler/2]).
-export([make_message/4]).

-include_lib("stdlib/include/qlc.hrl").
-include("../include/publicator_core.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts pub-sub server
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() ->
    ok = pc_utils:ensure_started(lager),
    ok = pc_utils:ensure_started(ibrowse),
    ok = pc_utils:ensure_started(publicator_core).


%%--------------------------------------------------------------------
%% @doc
%% Stops pub-sub server
%% 
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    ok = application:stop(publicator_core).



get_channels() ->
    {error, not_implemented}.
%%--------------------------------------------------------------------
%% @doc
%% Stops pub-sub server
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_messages(binary()) -> {ok, [tuple()]} | {error, producer_not_found}.
get_messages(Producer_code) ->
    case pc_producer:get(Producer_code) of
	{ok, Producer_pid} ->
	    pc_producer:get_messages(Producer_pid);
	{error, not_found} -> {error, producer_not_found}
    end.

-spec publish(#message{}) -> ok
                           | {error, producer_not_found}
                           | {error, permission_denied}.
publish(#message{producer_code=Producer_code}=Message)->
    case pc_producer:get(Producer_code) of
	{ok, Producer_pid} ->
            %% returns ok or permission denied
	    pc_producer:publish(Producer_pid, Message);
	{error, not_found} -> {error, producer_not_found}
    end.

-spec subscribe(code(), code(), message_meta()) ->
                       ok  | {error, invalid_channel_code}
                           | {error, producer_not_found}
                           | {error, permission_denied}.
subscribe(Producer_code, Channel_code, Meta) ->
    case is_channel_code_valid(Channel_code) of
	false -> {error, invalid_channel_code};
	true ->
	    case pc_producer:get(Producer_code) of
                {ok, Producer_pid} ->
                    pc_producer:subscribe(Producer_pid, Channel_code, Meta);
		{error, not_found} -> {error, producer_not_found}
	    end
    end.

-spec unsubscribe(binary(), binary()) -> ok | {error, producer_not_found}.
unsubscribe(Producer_code, Channel_code) ->
    case pc_producer:get(Producer_code) of
	{ok, Producer_pid} ->
	    ok = pc_producer:unsubscribe(Producer_pid, Channel_code);
	{error, not_found} -> {error, producer_not_found}
    end.

-spec get_subscribtions(binary()) -> {ok, dict:dict(binary(), pid())} |
                                     {error, producer_not_found}.
get_subscribtions(Producer_code) ->
    case pc_producer:get(Producer_code) of
	{ok, Producer_pid} ->
	    pc_producer:get_subscribtions(Producer_pid);
	{error, not_found} -> {error, producer_not_found}
    end.

-spec add_message_handler(binary(), pid()) -> ok.
add_message_handler(Producer_code, Handler_pid) ->
    case pc_producer:get(Producer_code) of
	{ok, Producer_pid} ->
	    pc_producer:add_message_handler(Producer_pid, Handler_pid);
	{error, not_found} -> {error, producer_not_found}
    end.

-spec remove_message_handler(binary(), pid()) -> ok.
remove_message_handler(Producer_code, Handler_pid) ->
    case pc_producer:get(Producer_code) of
	{ok, Producer_pid} ->
	    pc_producer:remove_message_handler(Producer_pid, Handler_pid);
	{error, not_found} -> {error, producer_not_found}
    end.


-spec create_producer(message_meta()) -> {ok, Code::binary(), Pid::pid()}
                                         | {error, permission_denied}.
create_producer(Meta) ->
    pc_producer_sup:start_child(Meta).

-spec stop_producer(binary()) -> ok|{error, producer_not_found}.
stop_producer(Producer_code) ->
    case pc_producer:get(Producer_code) of
	{ok, Producer_pid} ->
            pc_producer:stop(Producer_pid);
	{error, not_found} -> {error, producer_not_found}
    end.


-spec get_producer(binary()) -> {ok, pid()} | {error, not_found}.
get_producer(Producer_code) ->
    pc_producer:get(Producer_code).

-spec get_producers(binary(), binary(), list()) ->
                           {ok, [pid()]}
                               | {error, producer_not_found}
                               | {error, permission_denied}
                               | {error, invalid_channel_code}.
get_producers(Producer_code, Channel_code, Extra_data) ->
    case pc_producer:get(Producer_code) of
	{ok, Producer_pid} ->
            pc_producer:get_producers(Producer_pid, Channel_code, Extra_data);
	{error, not_found} -> {error, producer_not_found}
    end.

-spec make_message(binary(), binary(), binary(), #{}) -> #message{}.
make_message(Producer_code, Channel_code, Data, Meta) ->
        pc_utils:make_message(Producer_code, Channel_code, message, Data, Meta).
    

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec is_channel_code_valid(binary()) -> boolean().
is_channel_code_valid(Channel_code) ->
    case re:run(Channel_code,"^[0-9a-z_]*$",[{capture, none}]) of
	match -> true;
	nomatch -> false
	end.
