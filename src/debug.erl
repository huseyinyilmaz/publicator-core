%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2015, Huseyin Yilmaz
%%% @doc
%%% Debugging code for publicator_core
%%% @end
%%% Created : 30 Aug 2015 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(debug).

%% API
-export([run/0]).

%%%===================================================================
%%% API
%%%===================================================================

run() ->
    Channel_code = <<"channel1">>,
    {ok, P1, _Pid1} = publicator_core:create_producer(#{}),
    lager:warning("P1 = ~p", [P1]),
    {ok, P2, _Pid2} = publicator_core:create_producer(#{}),
    lager:warning("P2 = ~p", [P2]),
    publicator_core:subscribe(P1, Channel_code, #{}),
    publicator_core:subscribe(P2, Channel_code, #{}),
    Msg = publicator_core:make_message(P1, Channel_code, <<"Test message">>, #{}),
    publicator_core:publish(Msg),
    timer:sleep(1000),
    lager:debug("Messages for P1 = ~p", [publicator_core:get_messages(P1)]),
    lager:debug("Messages for P2 = ~p", [publicator_core:get_messages(P2)]).





%%%===================================================================
%%% Internal functions
%%%===================================================================
