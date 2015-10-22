%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2015, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 24 Aug 2015 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(publicator_core_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("../include/publicator_core.hrl").

-define(CHANNEL1, <<"channelcode1">>).
-define(CHANNEL2, <<"channelcode2">>).
-define(MESSAGE1, <<"message1">>).
-define(MESSAGE2, <<"message2">>).
-define(META, #{}).
-define(DELAY, 500).

-define(PERMISSION_CONFIG,
        {publicator_static_permission_backend,
         [[{producer_code, all},
           {extra_data, []},
           {channel_code, all},
           {publish, true},
           {subscribe, true},
           {create, true},
           {listen_events, true}]]}).

-define(CANNOT_PUBLISH_PERMISSION_CONFIG,
        {publicator_static_permission_backend,
         [[{producer_code, all},
           {extra_data, []},
           {channel_code, all},
           {publish, false},
           {subscribe, true},
           {create, true},
           {listen_events, true}]]}).

-define(PERSISTENCE_CONFIG,
        {publicator_inmemmory_persistence_backend, []}).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(permission_integration_test_case, Config) ->
    lager:start(),
    pc_utils:set_env(publicator_core, permission_backend, ?CANNOT_PUBLISH_PERMISSION_CONFIG),
    pc_utils:set_env(publicator_core, persistence_backend, ?PERSISTENCE_CONFIG),
    ok = publicator_core:start(),
    Config;
init_per_testcase(_TestCase, Config) ->
    lager:start(),
    pc_utils:set_env(publicator_core, permission_backend, ?PERMISSION_CONFIG),
    pc_utils:set_env(publicator_core, persistence_backend, ?PERSISTENCE_CONFIG),
    ok = publicator_core:start(),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
%%    lager:info("Cleanup server"),
    ok = publicator_core:stop(),
    application:stop(lager),
    application:stop(goldrush),
    ok.

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [uninitialized_producer_test_case,
     subscribtion_test_case,
     send_message_test_case,
     channel_seperation_test_case,
     receive_message_test_case,
     permission_integration_test_case,
     eunit_utils_test_case
    ].

uninitialized_producer_test_case(_Config) ->
    Consumer_code = <<"no-producer">>,
    Channel_code = ?CHANNEL1,
    Msg = publicator_core:make_message(Consumer_code, Channel_code, ?MESSAGE1, ?META),
    %% test uninitialized sesssions
    {error, producer_not_found} = publicator_core:get_messages(Consumer_code),
    {error, producer_not_found} = publicator_core:publish(Msg),
    {error, producer_not_found} = publicator_core:subscribe(Consumer_code, Channel_code, ?META),
    {error, producer_not_found} = publicator_core:unsubscribe(Consumer_code, Channel_code),
    {error, producer_not_found} = publicator_core:get_subscribtions(Consumer_code),
    ok.

subscribtion_test_case(_Config) ->
    Channel_code = ?CHANNEL1,
    Channel_code2 = ?CHANNEL2,
    % create producer
    {ok, Producer_code1, _} = publicator_core:create_producer(?META),
    {ok, Producer_code2, _} = publicator_core:create_producer(?META),
    % Subscribe to channels
    ok = publicator_core:subscribe(Producer_code1, Channel_code, ?META),
    ok = publicator_core:subscribe(Producer_code1, Channel_code2, ?META),
    ok = publicator_core:subscribe(Producer_code2, Channel_code, ?META),
    % assert Channel has expected producers.
    {ok, Producer_list1} = publicator_core:get_producers(Producer_code1, Channel_code, ?META),
    Expected_producer_list1 = lists:sort([Producer_code1, Producer_code2]),
    Expected_producer_list1 = lists:sort(Producer_list1),
    % assert producers is subscrived to expected channels.
    {ok, [Channel_code2, Channel_code]} = publicator_core:get_subscribtions(Producer_code1),
    {ok, [Channel_code]} = publicator_core:get_subscribtions(Producer_code2),
    % unsubscribe both producers from channel1
    ok = publicator_core:unsubscribe(Producer_code1, Channel_code),
    ok = publicator_core:unsubscribe(Producer_code2, Channel_code),
    % check if producer1 has one subscribtion left and producer 2 has none left.
    {ok,[Channel_code2]} = publicator_core:get_subscribtions(Producer_code1),
    {ok,[]} = publicator_core:get_subscribtions(Producer_code2),
    % remove remaining subscribtion from producer one and check if it is gone.
     ok = publicator_core:unsubscribe(Producer_code2, Channel_code2),
    {ok,[]} = publicator_core:get_subscribtions(Producer_code2),
    ok.

send_message_test_case(_Config) ->
    Channel_code = ?CHANNEL1,
    % create producer
    {ok, Producer_code1, _} = publicator_core:create_producer(?META),
    {ok, Producer_code2, _} = publicator_core:create_producer(?META),
    timer:sleep(?DELAY),
    % create messages
    Msg1 = publicator_core:make_message(Producer_code1, Channel_code, ?MESSAGE1, ?META),
    Msg2 = publicator_core:make_message(Producer_code2, Channel_code, ?MESSAGE2, ?META),
    Subscribe_msg = #message{type=add_subscribtion,
                             channel_code=Channel_code,
                             producer_code=Producer_code2},
    % subscribe both producers to channel1
    ok = publicator_core:subscribe(Producer_code1, Channel_code, ?META),
    ok = publicator_core:subscribe(Producer_code2, Channel_code, ?META),
    % publish messages
    ok = publicator_core:publish(Msg1),
    ok = publicator_core:publish(Msg2),
    timer:sleep(?DELAY),
    % check producer messages.
    {ok, [Subscribe_msg, Msg1, Msg2]} = publicator_core:get_messages(Producer_code1),
    {ok, [Msg1, Msg2]} = publicator_core:get_messages(Producer_code2),
    % make sure that messages has been cleared
    {ok, []} = publicator_core:get_messages(Producer_code1),
    {ok, []} = publicator_core:get_messages(Producer_code2),
    ok.

channel_seperation_test_case(_Config) ->
    Channel_code = ?CHANNEL1,
    Channel_code2 = ?CHANNEL2,
    % create producer
    {ok, Producer_code1, _} = publicator_core:create_producer(?META),
    {ok, Producer_code2, _} = publicator_core:create_producer(?META),
    timer:sleep(?DELAY),
    % create messages
    Msg1 = publicator_core:make_message(Producer_code1, Channel_code, ?MESSAGE1, ?META),
    Msg2 = publicator_core:make_message(Producer_code1, Channel_code, ?MESSAGE2, ?META),
    % subscribe both producers to different channels
    ok = publicator_core:subscribe(Producer_code1, Channel_code, ?META),
    ok = publicator_core:subscribe(Producer_code2, Channel_code2, ?META),
    % publish messages
    ok = publicator_core:publish(Msg1),
    ok = publicator_core:publish(Msg2),
    timer:sleep(?DELAY),
    % make sure only one of them receives messages
    {ok, [Msg1, Msg2]} = publicator_core:get_messages(Producer_code1),
    {ok, []} = publicator_core:get_messages(Producer_code2),
    ok.

receive_message_test_case(_Config) ->
    Channel_code = ?CHANNEL1,
    % create producer
    {ok, Producer_code1, _} = publicator_core:create_producer(?META),
    {ok, Producer_code2, _} = publicator_core:create_producer(?META),
    timer:sleep(?DELAY),
    % create messages
    Msg1 = publicator_core:make_message(Producer_code1, Channel_code, ?MESSAGE1, ?META),
    Msg2 = publicator_core:make_message(Producer_code2, Channel_code, ?MESSAGE2, ?META),
    % Subscribe all producers to all channels
    ok = publicator_core:subscribe(Producer_code1, Channel_code, ?META),
    ok = publicator_core:subscribe(Producer_code2, Channel_code, ?META),
    % create 3 listeners.
    {ok, Mock1} = process_mock:start_link(),
    {ok, Mock2} = process_mock:start_link(),
    % connect listeners to producers.
    publicator_core:add_message_handler(Producer_code1, Mock1),
    publicator_core:add_message_handler(Producer_code2, Mock2),
    % publish messages
    publicator_core:publish(Msg1),
    timer:sleep(?DELAY),
    publicator_core:publish(Msg2),
    % make sure listeners got corect_messages
    true = ctcheck:equal({ok, [Msg2, Msg1]}, process_mock:get_messages(Mock1, 2)),
    true = ctcheck:equal({ok, [Msg2, Msg1]}, process_mock:get_messages(Mock2, 2)),
    ok.

permission_integration_test_case(_Config) ->
    Channel_code = ?CHANNEL1,
    % create producer
    {ok, Producer_code1, _} = publicator_core:create_producer(?META),
    timer:sleep(?DELAY),
    % create messages
    Msg1 = publicator_core:make_message(Producer_code1, Channel_code, ?MESSAGE1, ?META),
    % Subscribe all producers to all channels
    ok = publicator_core:subscribe(Producer_code1, Channel_code, ?META),
    true = ctcheck:equal({error,permission_denied}, publicator_core:publish(Msg1)),
    ok.

eunit_utils_test_case(_Config) ->
    ok = eunit:test(utils_tests).
