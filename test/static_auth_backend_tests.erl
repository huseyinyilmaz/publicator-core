-module(static_auth_backend_tests).
-include_lib("eunit/include/eunit.hrl").

-include("../include/publicator_core.hrl").

-export([setup_server/0, cleanup_server/1]).
-export([setup_server_open_all_permissions/0]).
-export([setup_server_close_all_permissions/0]).

-define(CHANNEL1, <<"channelcode1">>).
-define(CHANNEL2, <<"channelcode2">>).
-define(MESSAGE1, <<"message1">>).
-define(MESSAGE2, <<"message2">>).
-define(AUTH_INFO, <<"test_auth_code">>).
-define(EXTRA_DATA, []).

-define(PERMISSION_CONFIG,
        {publicator_static_permission_backend,
         [[{consumer_code, all},
           {extra_data, []},
           {channel_code, all},
           {can_publish, true},
           {can_subscribe_messages, true},
           {can_subscribe_all_events, true},
           {can_create_channel, true}]]}).
-define(DELAY, 100).

setup_server() ->
    lager:start(),
    lager:debug("Setup server"),
    ok = publicator_core:start().

setup_server_open_all_permissions() ->
    Configuration = {publicator_static_auth_backend,
                     [[{consumer_code, all},
                       {auth_info, all},
                       {extra_data, []}]]},
    pc_utils:set_env(server, auth_backend, Configuration),
    pc_utils:set_env(server, permission_backend, ?PERMISSION_CONFIG),
    setup_server().

setup_server_close_all_permissions() ->
    Configuration = {publicator_static_auth_backend,
                     [[{consumer_code, <<"closed">>},
                       {auth_info, <<"closed">>},
                       {extra_data}, [{<<"some_data">>, <<"some_value">>}]]]},
    pc_utils:set_env(server, auth_backend, Configuration),
    pc_utils:set_env(server, permission_backend, ?PERMISSION_CONFIG),
    setup_server().

cleanup_server(_) ->
    lager:debug("Cleanup server"),
    ok = publicator_core:stop(),
    application:stop(lager),
    application:stop(goldrush).

server_opened_auth_test_() ->
    {setup,
     fun setup_server_open_all_permissions/0,
     fun cleanup_server/1,
     {"Test all permissions enabled.",
      ?_test(
         begin
	     {ok, Consumer_code1, _} = publicator_core:create_consumer(?AUTH_INFO, ?EXTRA_DATA),
             ?assertEqual(ok, publicator_core:subscribe(Consumer_code1, ?CHANNEL1, message_only,[])),
             ok = publicator_core:publish(Consumer_code1, ?CHANNEL1, ?MESSAGE1, ?EXTRA_DATA),
             timer:sleep(?DELAY),
	     {ok, Messages} = publicator_core:get_messages(Consumer_code1),
	     ?assertEqual([#message{type=message,
                                    data=?MESSAGE1,
                                    channel_code=?CHANNEL1}], Messages)
         end)
     }}.

server_closed_auth_test_() ->
    {setup,
     fun setup_server_close_all_permissions/0,
     fun cleanup_server/1,
     {"Test all permissions disabled.",
      ?_test(
         begin
	     {error,permission_denied} = publicator_core:create_consumer(?AUTH_INFO, ?EXTRA_DATA)
         end)
     }}.