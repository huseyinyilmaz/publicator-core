-module(utils_tests).
-include_lib("eunit/include/eunit.hrl").

-include("../include/publicator_core.hrl").

make_message_test () ->
    Channel_code = <<"channel_code">>,
    Producer_code = <<"producer_code">>,
    Data = <<"message">>,
    Extra_data = <<"extra_data">>,
    Cache = true,
    ?assertEqual(
        #message{
           producer_code=Producer_code,
           channel_code=Channel_code,
           type=message,
           data=Data,
           meta=#message_meta{cache=Cache,
                              extra_data=Extra_data}},
       publicator_core:make_message(Producer_code, Channel_code, Data, Extra_data, Cache)).
