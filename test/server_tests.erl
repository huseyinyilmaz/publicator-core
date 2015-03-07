-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

-include("../include/publicator_core.hrl").

-export([setup_server/0, cleanup_server/1]).

-define(CONSUMER1, <<"consumercode1">>).
-define(CONSUMER2, <<"consumercode2">>).
-define(CHANNEL1, <<"channelcode1">>).
-define(CHANNEL2, <<"channelcode2">>).
-define(MESSAGE1, <<"message1">>).
-define(MESSAGE2, <<"message2">>).
-define(AUTH_INFO, <<"test_auth_code">>).
-define(EXTRA_DATA, []).
-define(DELAY, 100).

-define(PERMISSION_CONFIG,
        {publicator_static_permission_backend,
         [[{consumer_code, all},
           {extra_data, []},
           {channel_code, all},
           {can_publish, true},
           {can_subscribe_messages, true},
           {can_subscribe_all_events, true},
           {can_create_channel, true}]]}).

setup_server()->
    lager:start(),
    lager:info("Setup server"),
    Configuration = {publicator_static_auth_backend,
                     [[{consumer_code, all},
                       {auth_info, all},
                       {extra_data, []}]]},
    pc_utils:set_env(publicator_core, auth_backend, Configuration),
    pc_utils:set_env(publicator_core, permission_backend, ?PERMISSION_CONFIG),
    ok = publicator_core:start().

cleanup_server(_)->
    lager:info("Cleanup server"),
    ok = publicator_core:stop(),
    application:stop(lager),
    application:stop(goldrush).

server_uninitialized_session_test_()->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test uninitialized session functionality",
      ?_test(
	 begin
	     Consumer_code = ?CONSUMER1,
	     Channel_code = ?CHANNEL1,
	     %% test uninitialized sesssions
             ?assertEqual({error, consumer_not_found},
			  publicator_core:get_messages(Consumer_code)),
             ?assertEqual({error, consumer_not_found},
			  publicator_core:publish(Consumer_code, Channel_code, ?MESSAGE1, ?EXTRA_DATA)),
	     ?assertEqual({error, consumer_not_found},
			  publicator_core:subscribe(Consumer_code, Channel_code,
                                           message_only, ?EXTRA_DATA)),
	     ?assertEqual({error, consumer_not_found},
                          publicator_core:unsubscribe(Consumer_code, Channel_code)),
             ?assertEqual({error, consumer_not_found},
			  publicator_core:get_subscribtions(Consumer_code)),
             ok
	 end)
     }}.


server_subscribtion_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test subscribe unsubscribe functionality",
      ?_test(
	 begin
	     {ok, Consumer_code1, _} = publicator_core:create_consumer(?AUTH_INFO, ?EXTRA_DATA),
	     {ok, Consumer_code2, _} = publicator_core:create_consumer(?AUTH_INFO, ?EXTRA_DATA),
	     Channel_code = ?CHANNEL1,
	     Channel_code2 = ?CHANNEL2,
             %% tests subscribe
	     %% multiple subscribtions to same Channel should not create multiple
	     %% channel event handlers
             ?assertEqual(ok,
                          publicator_core:subscribe(Consumer_code1,
                                           Channel_code,
                                           message_only,
                                           ?EXTRA_DATA)),
             ?assertEqual(ok, publicator_core:subscribe(Consumer_code1,
                                               Channel_code2,
                                               message_only,
                                               ?EXTRA_DATA)),
             ?assertEqual(ok, publicator_core:subscribe(Consumer_code2,
                                               Channel_code,
                                               message_only,
                                               ?EXTRA_DATA)),
	     {ok, Consumer_list1} = publicator_core:get_consumers(Consumer_code1,
                                                         Channel_code, ?EXTRA_DATA),
             ?assertEqual(lists:sort([Consumer_code1, Consumer_code2]),
			  lists:sort(Consumer_list1)),
             ?assertEqual({ok, [Consumer_code1]},
			  publicator_core:get_consumers(Consumer_code1, Channel_code2, ?EXTRA_DATA)),
	     
	     %% test get channels
	     %% {ok, Channel_list} = publicator_core:get_channels(),
             %% ?assertEqual(lists:sort([Channel_code, Channel_code2]),
	     %%    	  lists:sort(Channel_list)),
             
	     %% test get subscribtions
             ?assertEqual({ok,[Channel_code2, Channel_code]},
			  publicator_core:get_subscribtions(Consumer_code1)),
	     ?assertEqual({ok,[Channel_code]},
			  publicator_core:get_subscribtions(Consumer_code2)),
             %% tests unsubscribe
             ?assertEqual(ok, publicator_core:unsubscribe(Consumer_code1, Channel_code)),
             ?assertEqual(ok, publicator_core:unsubscribe(Consumer_code2, Channel_code)),
             
	     %% {ok, Channel_list2} = publicator_core:get_channels(),
             %% ?assertEqual(lists:sort([Channel_code, Channel_code2]),
	     %%    	  lists:sort(Channel_list2)),

             ?assertEqual({ok,[Channel_code2]}, publicator_core:get_subscribtions(Consumer_code1)),
             ?assertEqual({ok,[]}, publicator_core:get_subscribtions(Consumer_code2)),
             ?assertEqual(ok, publicator_core:unsubscribe(Consumer_code2, Channel_code2)),
             ?assertEqual({ok,[]}, publicator_core:get_subscribtions(Consumer_code2)),
             ok
	 end)
     }}.
		 
server_message_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test publish get_message functionality",
      ?_test(
	 begin
	     {ok, Consumer_code1, _} = publicator_core:create_consumer(?AUTH_INFO, ?EXTRA_DATA),
	     {ok, Consumer_code2, _} = publicator_core:create_consumer(?AUTH_INFO, ?EXTRA_DATA),
	     Channel_code = ?CHANNEL1,
	     Channel_code2 = ?CHANNEL2,
	     timer:sleep(?DELAY),
             %% tests subscribe
	     %% multiple subscribtions to same Channel should not create multiple
	     %% channel event handlers
             ?assertEqual(ok, publicator_core:subscribe(Consumer_code1, Channel_code, message_only, ?EXTRA_DATA)),
             ?assertEqual(ok, publicator_core:subscribe(Consumer_code2, Channel_code, message_only, ?EXTRA_DATA)),
	     %% test send message
             ok = publicator_core:publish(Consumer_code1, Channel_code, ?MESSAGE1, ?EXTRA_DATA),
             ok = publicator_core:publish(Consumer_code1, Channel_code, ?MESSAGE2, ?EXTRA_DATA),
	     timer:sleep(?DELAY),
	     %% test get_messages
	     {ok, Messages} = publicator_core:get_messages(Consumer_code2),
             Expected_messages = [#message{type=message,
                                           data=?MESSAGE1,
                                          channel_code=Channel_code},
                                  #message{type=message,
                                           data=?MESSAGE2,
                                          channel_code=Channel_code}],
	     ?assertEqual(Messages, Expected_messages),
	     {ok, Messages2} = publicator_core:get_messages(Consumer_code1),
	     ?assertEqual(Expected_messages, Messages2),
	     
	     %% make sure that Messages has been cleared
	     {ok, Messages3} = publicator_core:get_messages(Consumer_code2),
	     ?assertEqual(Messages3, []),
	     %% make usre that original sender did not get the messages
	     {ok, Messages4} = publicator_core:get_messages(Consumer_code1),
	     ?assertEqual(Messages4,[]),
	     %% make sure that channels are seperate
             ?assertEqual(ok, publicator_core:subscribe(Consumer_code2, Channel_code2, message_only, ?EXTRA_DATA)),
	     timer:sleep(?DELAY),
             ok = publicator_core:publish(Consumer_code1, Channel_code, ?MESSAGE1, ?EXTRA_DATA),
             ok = publicator_core:publish(Consumer_code1, Channel_code2, ?MESSAGE2, ?EXTRA_DATA),
	     timer:sleep(?DELAY),
             {ok, Messages5} = publicator_core:get_messages(Consumer_code2),
             Expected_messages5 = [#message{type=message,
                                            data=?MESSAGE1,
                                            channel_code=Channel_code},
                                   #message{type=message,
                                            data=?MESSAGE2,
                                            channel_code=Channel_code2}],
             
	     %% test get_messages single channel
             ?assertEqual(Messages5, Expected_messages5),
             %% tests unsubscribe
             ?assertEqual(ok, publicator_core:unsubscribe(Consumer_code1, Channel_code)),
             ?assertEqual(ok, publicator_core:unsubscribe(Consumer_code2, Channel_code))
	 end)
     }}.


server_handler_message_only_mode_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test consumers that has message_only handler processes",
      ?_test(begin
		 %% Create consumers
		 {ok, Consumer_code1, Consumer_pid1} = publicator_core:create_consumer(?AUTH_INFO, ?EXTRA_DATA),
		 {ok, Consumer_code2, Consumer_pid2} = publicator_core:create_consumer(?AUTH_INFO, ?EXTRA_DATA),
		 Channel_code = ?CHANNEL1,
		 Channel_code2 = ?CHANNEL2,
		 Message1 = <<"Message1">>,
		 Message2 = <<"Message2">>,
		 timer:sleep(?DELAY),
		 %% Add consumers to channels

		 ?assertEqual(ok,
			      publicator_core:subscribe(Consumer_code1, Channel_code, message_only, ?EXTRA_DATA)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Consumer_code2, Channel_code, message_only, ?EXTRA_DATA)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Consumer_code1, Channel_code2, message_only, ?EXTRA_DATA)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Consumer_code2, Channel_code2, message_only, ?EXTRA_DATA)),

		 Mock1_pid = process_mock:make_message_receiver(self(), mock1),
		 Mock2_pid = process_mock:make_message_receiver(self(), mock2),
		 Mock3_pid = process_mock:make_message_receiver(self(), mock3),
		 Mock4_pid = process_mock:make_message_receiver(self(), mock4),

		 publicator_core:add_message_handler(Consumer_code1, Mock1_pid),
		 publicator_core:add_message_handler(Consumer_code2, Mock2_pid),
		 publicator_core:add_message_handler(Consumer_code2, Mock3_pid),
		 publicator_core:add_message_handler(Consumer_code2, Mock4_pid),

		 Expected_msg1 = #message{type=message,
                                          data=Message1,
                                          channel_code=Channel_code},
		 Expected_msg2 = #message{type=message,
                                          data=Message2,
                                          channel_code=Channel_code2},

		 pc_consumer:publish(Consumer_pid1, Channel_code, Message1, ?EXTRA_DATA),
		 timer:sleep(?DELAY),
		 pc_consumer:publish(Consumer_pid2, Channel_code2, Message2, ?EXTRA_DATA),
		 timer:sleep(?DELAY),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock2)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock3)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock4)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock1)),
		 ?assertEqual(Expected_msg2, process_mock:receive_message(mock1))
	     end)}}.


server_all_handler_mode_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test consumers that has all handler processes",
      ?_test(begin
		 %% Create consumers
		 {ok, Consumer_code1, Consumer_pid1} = publicator_core:create_consumer(?AUTH_INFO, ?EXTRA_DATA),
		 {ok, Consumer_code2, Consumer_pid2} = publicator_core:create_consumer(?AUTH_INFO, ?EXTRA_DATA),
		 Channel_code = ?CHANNEL1,
		 Channel_code2 = ?CHANNEL2,
		 Message1 = <<"Message1">>,
		 Message2 = <<"Message2">>,
		 timer:sleep(?DELAY),
		 Mock1_pid = process_mock:make_message_receiver(self(), mock1),
		 Mock2_pid = process_mock:make_message_receiver(self(), mock2),
		 Mock3_pid = process_mock:make_message_receiver(self(), mock3),
		 Mock4_pid = process_mock:make_message_receiver(self(), mock4),

		 publicator_core:add_message_handler(Consumer_code1, Mock1_pid),
		 publicator_core:add_message_handler(Consumer_code2, Mock2_pid),
		 publicator_core:add_message_handler(Consumer_code2, Mock3_pid),
		 publicator_core:add_message_handler(Consumer_code2, Mock4_pid),
		 %% Add consumers to channels
		 ?assertEqual(ok,
			      publicator_core:subscribe(Consumer_code1, Channel_code, all, ?EXTRA_DATA)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Consumer_code2, Channel_code, all, ?EXTRA_DATA)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Consumer_code1, Channel_code2, all, ?EXTRA_DATA)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Consumer_code2, Channel_code2, all, ?EXTRA_DATA)),
		 %% Receive add_subscribtion messages.
		 Expected_add_subscribtion_msg1 = #message{type=add_subscribtion,
                                                           data=Consumer_code2,
                                                           channel_code=Channel_code},
		 Expected_add_subscribtion_msg2 = #message{type=add_subscribtion,
                                                           data=Consumer_code2,
                                                           channel_code=Channel_code2},
                 
		 ?assertEqual(Expected_add_subscribtion_msg1,
			      process_mock:receive_message(mock1)),
		 
		 ?assertEqual(Expected_add_subscribtion_msg2,
			      process_mock:receive_message(mock1)),

		 %% publish messages
		 Expected_msg1 = #message{type=message,
                                          data=Message1,
                                          channel_code=Channel_code},
		 Expected_msg2 = #message{type=message,
                                          data=Message2,
                                          channel_code=Channel_code2},

		 pc_consumer:publish(Consumer_pid1, Channel_code, Message1, ?EXTRA_DATA),
		 pc_consumer:publish(Consumer_pid2, Channel_code2, Message2, ?EXTRA_DATA),
		 timer:sleep(?DELAY),
		 %% receive messages
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock2)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock3)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock4)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock1)),
		 ?assertEqual(Expected_msg2, process_mock:receive_message(mock1)),


		 %% Receive add_subscribtion messages.
		 Expected_remove_subscribtion_msg1 = #message{type=remove_subscribtion,
                                                              data=Consumer_code2,
                                                              channel_code=Channel_code},
		 Expected_remove_subscribtion_msg2 = #message{type=remove_subscribtion,
                                                              data=Consumer_code2,
                                                              channel_code=Channel_code2},

		 
		 ?assertEqual(ok,
			      publicator_core:unsubscribe(Consumer_code2, Channel_code)),
		 ?assertEqual(ok,
			      publicator_core:unsubscribe(Consumer_code2, Channel_code2)),
		 
		 ?assertEqual(Expected_remove_subscribtion_msg1,
			      process_mock:receive_message(mock1)),
		 ?assertEqual(Expected_remove_subscribtion_msg2,
			      process_mock:receive_message(mock1))
		     
	     end)}}.
