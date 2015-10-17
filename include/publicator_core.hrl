-type code()::binary().
-type channel_handler_type()::message_only | all.
-type interface_type()::all|atom().
-type message_type() :: message|add_subscribtion|remove_subscribtion.
-type message_meta() :: #{binary() => binary()}.

-record(permissions, {create::boolean(),
                      publish::boolean(),
                      subscribe::boolean(),
                      listen_events::boolean()}).

-record(message, {producer_code :: code(),
                  channel_code :: code(),
                  type :: message_type(),
                  data = undefined :: binary(),
                  meta = #{} :: message_meta()}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PERSISTENCE_BACKEND CONFIGURATION %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(DEFAULT_MESSAGE_COUNT, 20).

-define(DEFAULT_PERSISTENCE_ARG,
        [{channel_code, all}, {message_count, ?DEFAULT_MESSAGE_COUNT}]).
