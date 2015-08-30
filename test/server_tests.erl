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
-define(META, #{}).
-define(DELAY, 500).

-define(PERMISSION_CONFIG,
        {publicator_static_permission_backend,
         [[{consumer_code, all},
           {meta, #{}},
           {channel_code, all},
           {can_publish, true},
           {can_subscribe_messages, true},
           {can_subscribe_all_events, true},
           {can_create_channel, true},
           {publish_info_messages, true}]]}).

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
	     Producer_code = ?CONSUMER1,
	     Channel_code = ?CHANNEL1,
             Msg1 = #message{type=message,
                             data=?MESSAGE1,
                             channel_code=Channel_code,
                             producer_code=Producer_code},

	     %% test uninitialized sesssions
             ?assertEqual({error, producer_not_found},
			  publicator_core:get_messages(Producer_code)),
             ?assertEqual({error, producer_not_found},
			  publicator_core:publish(Msg1)),
	     ?assertEqual({error, producer_not_found},
			  publicator_core:subscribe(Producer_code, Channel_code, ?META)),
	     ?assertEqual({error, producer_not_found},
                          publicator_core:unsubscribe(Producer_code, Channel_code)),
             ?assertEqual({error, producer_not_found},
			  publicator_core:get_subscribtions(Producer_code)),
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
	     {ok, Producer_code1, _} = publicator_core:create_producer(?META),
	     {ok, Producer_code2, _} = publicator_core:create_producer(?META),
	     Channel_code = ?CHANNEL1,
	     Channel_code2 = ?CHANNEL2,
             %% tests subscribe
	     %% multiple subscribtions to same Channel should not create multiple
	     %% channel event handlers
             ?assertEqual(ok,
                          publicator_core:subscribe(Producer_code1,
                                                    Channel_code,
                                                    ?META)),
             ?assertEqual(ok, publicator_core:subscribe(Producer_code1,
                                                        Channel_code2,
                                                        ?META)),
             ?assertEqual(ok, publicator_core:subscribe(Producer_code2,
                                                        Channel_code,
                                                        ?META)),
	     {ok, Producer_list1} = publicator_core:get_producers(Producer_code1,
                                                                  Channel_code,
                                                                  ?META),
             ?assertEqual(lists:sort([Producer_code1, Producer_code2]),
			  lists:sort(Producer_list1)),
             ?assertEqual({ok, [Producer_code1]},
			  publicator_core:get_producers(Producer_code1, Channel_code2, ?META)),
	     
	     %% test get channels
	     %% {ok, Channel_list} = publicator_core:get_channels(),
             %% ?assertEqual(lists:sort([Channel_code, Channel_code2]),
	     %%    	  lists:sort(Channel_list)),
             
	     %% test get subscribtions
             ?assertEqual({ok,[Channel_code2, Channel_code]},
			  publicator_core:get_subscribtions(Producer_code1)),
	     ?assertEqual({ok,[Channel_code]},
			  publicator_core:get_subscribtions(Producer_code2)),
             %% tests unsubscribe
             ?assertEqual(ok, publicator_core:unsubscribe(Producer_code1, Channel_code)),
             ?assertEqual(ok, publicator_core:unsubscribe(Producer_code2, Channel_code)),
             
	     %% {ok, Channel_list2} = publicator_core:get_channels(),
             %% ?assertEqual(lists:sort([Channel_code, Channel_code2]),
	     %%    	  lists:sort(Channel_list2)),

             ?assertEqual({ok,[Channel_code2]}, publicator_core:get_subscribtions(Producer_code1)),
             ?assertEqual({ok,[]}, publicator_core:get_subscribtions(Producer_code2)),
             ?assertEqual(ok, publicator_core:unsubscribe(Producer_code2, Channel_code2)),
             ?assertEqual({ok,[]}, publicator_core:get_subscribtions(Producer_code2)),
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
	     {ok, Producer_code1, _} = publicator_core:create_producer(?META),
	     {ok, Producer_code2, _} = publicator_core:create_producer(?META),
	     Channel_code = ?CHANNEL1,
	     Channel_code2 = ?CHANNEL2,
	     timer:sleep(?DELAY),
             %% tests subscribe
	     %% multiple subscribtions to same Channel should not create multiple
	     %% channel event handlers
             ?assertEqual(ok, publicator_core:subscribe(Producer_code1, Channel_code, ?META)),
             ?assertEqual(ok, publicator_core:subscribe(Producer_code2, Channel_code, ?META)),
	     %% test send message
             Msg1 = #message{type=message,
                             data=?MESSAGE1,
                             channel_code=Channel_code,
                             producer_code=Producer_code1},
             Msg2 = #message{type=message,
                             data=?MESSAGE2,
                             channel_code=Channel_code,
                            producer_code=Producer_code2},
             Subscribe_msg = #message{
                                type=add_subscribtion,
                                channel_code=Channel_code,
                                producer_code=Producer_code2},
             ok = publicator_core:publish(Msg1),
             ok = publicator_core:publish(Msg2),
	     timer:sleep(?DELAY),
	     %% test get_messages
	     {ok, Messages} = publicator_core:get_messages(Producer_code2),
	     ?assertEqual(Messages, [Msg1, Msg2]),
	     {ok, Messages2} = publicator_core:get_messages(Producer_code1),
	     ?assertEqual(Messages2, [Subscribe_msg, Msg1, Msg2]),
	     
	     %% make sure that Messages has been cleared
	     {ok, Messages3} = publicator_core:get_messages(Producer_code2),
	     ?assertEqual(Messages3, []),
	     %% make usre that original sender did not get the messages
	     {ok, Messages4} = publicator_core:get_messages(Producer_code1),
	     ?assertEqual(Messages4,[]),
	     %% make sure that channels are seperate
             ?assertEqual(ok, publicator_core:subscribe(Producer_code2, Channel_code2, ?META)),
	     timer:sleep(?DELAY),
             ok = publicator_core:publish(Msg1),
             ok = publicator_core:publish(Msg2),
	     timer:sleep(?DELAY),
             {ok, Messages5} = publicator_core:get_messages(Producer_code2),
             Expected_messages5 = [Msg1, Msg2],
             
	     %% test get_messages single channel
             ?assertEqual(Messages5, Expected_messages5),
             %% tests unsubscribe
             ?assertEqual(ok, publicator_core:unsubscribe(Producer_code1, Channel_code)),
             ?assertEqual(ok, publicator_core:unsubscribe(Producer_code2, Channel_code))
	 end)
     }}.


server_handler_message_only_mode_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test consumers that has message_only handler processes",
      ?_test(begin
		 %% Create consumers
		 {ok, Producer_code1, Producer_pid1} = publicator_core:create_producer(?META),
		 {ok, Producer_code2, Producer_pid2} = publicator_core:create_producer(?META),
		 Channel_code = ?CHANNEL1,
		 Channel_code2 = ?CHANNEL2,
		 Message1 = <<"Message1">>,
		 Message2 = <<"Message2">>,
		 timer:sleep(?DELAY),
		 %% Add consumers to channels

		 ?assertEqual(ok,
			      publicator_core:subscribe(Producer_code1, Channel_code, ?META)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Producer_code2, Channel_code, ?META)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Producer_code1, Channel_code2, ?META)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Producer_code2, Channel_code2, ?META)),

		 Mock1_pid = process_mock:make_message_receiver(self(), mock1),
		 Mock2_pid = process_mock:make_message_receiver(self(), mock2),
		 Mock3_pid = process_mock:make_message_receiver(self(), mock3),
		 Mock4_pid = process_mock:make_message_receiver(self(), mock4),

		 publicator_core:add_message_handler(Producer_code1, Mock1_pid),
		 publicator_core:add_message_handler(Producer_code2, Mock2_pid),
		 publicator_core:add_message_handler(Producer_code2, Mock3_pid),
		 publicator_core:add_message_handler(Producer_code2, Mock4_pid),

		 Expected_msg1 = #message{type=message,
                                          data=Message1,
                                          producer_code=Producer_code1,
                                          channel_code=Channel_code},
		 Expected_msg2 = #message{type=message,
                                          producer_code=Producer_code2,
                                          data=Message2,
                                          channel_code=Channel_code2},

		 pc_producer:publish(Producer_pid1, Expected_msg1),
		 timer:sleep(?DELAY),
		 pc_producer:publish(Producer_pid2, Expected_msg2),
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
		 {ok, Producer_code1, Producer_pid1} = publicator_core:create_producer(?META),
		 {ok, Producer_code2, Producer_pid2} = publicator_core:create_producer(?META),
		 Channel_code = ?CHANNEL1,
		 Channel_code2 = ?CHANNEL2,
		 Message1 = <<"Message1">>,
		 Message2 = <<"Message2">>,
		 timer:sleep(?DELAY),
		 Mock1_pid = process_mock:make_message_receiver(self(), mock1),
		 Mock2_pid = process_mock:make_message_receiver(self(), mock2),
		 Mock3_pid = process_mock:make_message_receiver(self(), mock3),
		 Mock4_pid = process_mock:make_message_receiver(self(), mock4),

		 publicator_core:add_message_handler(Producer_code1, Mock1_pid),
		 publicator_core:add_message_handler(Producer_code2, Mock2_pid),
		 publicator_core:add_message_handler(Producer_code2, Mock3_pid),
		 publicator_core:add_message_handler(Producer_code2, Mock4_pid),
		 %% Add consumers to channels
		 ?assertEqual(ok,
			      publicator_core:subscribe(Producer_code1, Channel_code, ?META)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Producer_code2, Channel_code, ?META)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Producer_code1, Channel_code2, ?META)),
		 ?assertEqual(ok,
			      publicator_core:subscribe(Producer_code2, Channel_code2, ?META)),
		 %% Receive add_subscribtion messages.
		 Expected_add_subscribtion_msg1 = #message{type=add_subscribtion,
                                                           producer_code=Producer_code2,
                                                           channel_code=Channel_code},
		 Expected_add_subscribtion_msg2 = #message{type=add_subscribtion,
                                                           producer_code=Producer_code2,
                                                           channel_code=Channel_code2},
                 
		 ?assertEqual(Expected_add_subscribtion_msg1,
			      process_mock:receive_message(mock1)),
		 
		 ?assertEqual(Expected_add_subscribtion_msg2,
			      process_mock:receive_message(mock1)),

		 %% publish messages
		 Expected_msg1 = #message{type=message,
                                          data=Message1,
                                          producer_code=Producer_code1,
                                          channel_code=Channel_code},
		 Expected_msg2 = #message{type=message,
                                          data=Message2,
                                          producer_code=Producer_code2,
                                          channel_code=Channel_code2},

		 pc_producer:publish(Producer_pid1, Expected_msg1),
		 pc_producer:publish(Producer_pid2, Expected_msg2),
		 timer:sleep(?DELAY),

		 %% receive messages
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock2)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock3)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock4)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock1)),
		 ?assertEqual(Expected_msg2, process_mock:receive_message(mock1)),

		 %% Receive add_subscribtion messages.
		 Expected_remove_subscribtion_msg1 = #message{type=remove_subscribtion,
                                                              producer_code=Producer_code2,
                                                              channel_code=Channel_code},
		 Expected_remove_subscribtion_msg2 = #message{type=remove_subscribtion,
                                                              producer_code=Producer_code2,
                                                              channel_code=Channel_code2},

		 
		 ?assertEqual(ok,
			      publicator_core:unsubscribe(Producer_code2, Channel_code)),
		 ?assertEqual(ok,
			      publicator_core:unsubscribe(Producer_code2, Channel_code2)),

		 ?assertEqual(Expected_remove_subscribtion_msg1,
			      process_mock:receive_message(mock1)),
		 ?assertEqual(Expected_remove_subscribtion_msg2,
			      process_mock:receive_message(mock1)),
                 ok
	     end)}}.
