-type code()::binary().
-type channel_handler_type()::message_only | all.
-type interface_type()::all|atom().
-type permission_type()::{Channel_code::code()|all,
                          Dimension::interface_type(),
                          Publish::boolean(),
                          Subscribe::boolean(),
                          Can_create::boolean(),
                          Listen_all_room_events::boolean()}.


-type message_type() :: message|add_subscribtion|remove_subscribtion.
-type message_meta() :: #{binary() => binary()}.
-record(message, {producer_code :: code(),
                  channel_code :: code(),
                  type :: message_type(),
                  data = undefined :: binary(),
                  meta = #{} :: message_meta()}).
