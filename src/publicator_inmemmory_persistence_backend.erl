%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(publicator_inmemmory_persistence_backend).
-behivour(pc_persistence_backend).
%% API
-export([init_state/2, persist_message/2, get_cached_messages/1]).

-include("../include/publicator_core.hrl").

-record(state, {message_cache_size::integer(),
                current_message_cache_size::integer(),
                message_cache::queue:queue(term()),
                system_cache::queue:queue(term())}).


-spec init_state(Args::term(), Message_cache_size::integer()) -> New_state::term().

init_state(_Args, Message_cache_size) ->
    #state{message_cache_size=Message_cache_size,
           message_cache=queue:new()}.

persist_message(#message{type=message,
                         meta=#{cached:=true}}=Msg,
                #state{message_cache_size=Cache_size,
                       current_message_cache_size=Current_cache_size,
                       message_cache=Cache}=State) ->
    lager:debug("#######################"),
    lager:debug("Persist_message Msg=~p, State=~p", [Msg, State]),


    if
        Cache_size =< Current_cache_size ->
            Cache1 = queue:drop(queue:in(Msg, Cache)),
            Current_cache_size1 = Current_cache_size;
        Cache_size > Current_cache_size ->
            Cache1 = queue:in(Msg, Cache),
            Current_cache_size1 = Current_cache_size + 1
    end,
    State#state{current_message_cache_size=Current_cache_size1,
                message_cache=Cache1};

persist_message(_Message, State) -> State.

get_cached_messages(#state{message_cache=Message_cache}=State) ->
    lager:debug("#######################"),
    lager:debug("Get Cached Messages=~p", [queue:to_list(Message_cache)]),

    {queue:to_list(Message_cache), State}.
