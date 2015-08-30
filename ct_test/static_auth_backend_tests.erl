-module(static_auth_backend_tests).
-include_lib("eunit/include/eunit.hrl").

-include("../include/publicator_core.hrl").

-define(CHANNEL, <<"channelcode">>).
-define(META, #{}).

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

server_opened_auth_test() ->
    {ok, Producer_code1, _} = publicator_core:create_producer(?META),
    ?assertEqual(ok, publicator_core:subscribe(Producer_code1, ?CHANNEL, ?META)),
    Msg = #message{type=message,
                   data= <<"msg">>,
                   channel_code=?CHANNEL,
                   producer_code=Producer_code1},
    ok = publicator_core:publish(Msg),
    timer:sleep(?DELAY),
    {ok, Messages} = publicator_core:get_messages(Producer_code1),
    ?assertEqual([Msg], Messages).


server_closed_auth_test() ->
    Configuration = {publicator_static_auth_backend,
                     [[{consumer_code, <<"closed">>},
                       {auth_info, <<"closed">>},
                       {extra_data}, [{<<"some_data">>, <<"some_value">>}]]]},
    pc_utils:set_env(publicator_core, auth_backend, Configuration),
    pc_utils:set_env(publicator_core, permission_backend, ?PERMISSION_CONFIG),
    {error,permission_denied} = publicator_core:create_producer(?META).

