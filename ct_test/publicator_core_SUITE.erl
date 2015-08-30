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

-define(CHANNEL1, <<"channelcode1">>).
-define(CHANNEL2, <<"channelcode2">>).
-define(MESSAGE1, <<"message1">>).
-define(MESSAGE2, <<"message2">>).
-define(META, #{}).
-define(DELAY, 500).

-define(PERMISSION_CONFIG,
        {publicator_static_permission_backend,
         [[{consumer_code, all},
           {extra_data, []},
           {channel_code, all},
           {can_publish, true},
           {can_subscribe_messages, true},
           {can_subscribe_all_events, true},
           {can_create_channel, true}]]}).

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
init_per_testcase(_TestCase, Config) ->
    lager:start(),
%%    lager:info("Setup server"),
    Configuration = {publicator_static_auth_backend,
                     [[{consumer_code, all},
                       {auth_info, all},
                       {extra_data, []}]]},
    pc_utils:set_env(publicator_core, auth_backend, Configuration),
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
    [uninitialized_consumer_test_case,
     eunit_static_auth_backend_test_case
    ].

uninitialized_consumer_test_case(_Config) ->
    Consumer_code = <<"no-consumer">>,
    Channel_code = ?CHANNEL1,
    Msg = publicator_core:make_message(Consumer_code, Channel_code, ?MESSAGE1, ?META),
    %% test uninitialized sesssions
    {error, producer_not_found} = publicator_core:get_messages(Consumer_code),
    {error, producer_not_found} = publicator_core:publish(Msg),
    {error, producer_not_found} = publicator_core:subscribe(Consumer_code, Channel_code, ?META),
    {error, producer_not_found} = publicator_core:unsubscribe(Consumer_code, Channel_code),
    {error, producer_not_found} = publicator_core:get_subscribtions(Consumer_code),
    ok.

eunit_static_auth_backend_test_case(_Config) ->
    ok = eunit:test(static_auth_backend_tests).
