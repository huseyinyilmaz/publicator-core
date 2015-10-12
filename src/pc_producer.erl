%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 22 Jan 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(pc_producer).

-behaviour(gen_server).

-include("../include/publicator_core.hrl").

%% API
-export([start_link/2, get/1, get_code/1,
	 get_count/0, stop/1, push_message/2,
	 get_messages/1, subscribe/2,
	 publish/2, get_subscribtions/1, unsubscribe/2,
	 add_message_handler/2, remove_message_handler/2]).

-export([get_producers/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TIMEOUT, 1 * 60 * 1000).                % 1 minute

-record(state, {code :: binary(),                           % producer code
		channels :: dict:dict(binary(), pid()),          % producer's channel list
		messages :: queue:queue(binary()),          % messages dict (for rest interface)
                max_message_count :: number(),
                current_message_count :: number(),
		handlers :: [pid()],         % current listeners that will received messages
                permission_module :: atom(),
                permission_backend :: pid()
               }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts a new producer server
%% @end
%%--------------------------------------------------------------------
-spec start_link(code(), term()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Code, Meta) ->
    gen_server:start_link(?MODULE, [Code, Meta], []).

-spec get(binary()) -> {ok, pid()} | {error, not_found}.
get(Code) ->
    %% check if ets table has given Pid
    %% if it doesn't or value is a dead process
    %% set value to undefined.
    case pc_global:get_producer(Code) of
	undefined -> {error, not_found};
	Pid when is_pid(Pid) -> {ok, Pid}
    end.

-spec get_producers(Pid::pid(),
                   Channel_code::pid())->
                           {ok, list()}
                               | {error, permission_denied}
                               | {error, invalid_channel_code}.
get_producers(Pid, Channel_code)->
    gen_server:call(Pid,{get_producers, Channel_code}).

-spec get_code(pid()) -> {ok, binary()}.
get_code(Pid) ->
    gen_server:call(Pid, get_code).

-spec get_messages(pid()) -> {ok, [tuple()]}.
get_messages(Pid) ->
    gen_server:call(Pid, get_messages).

-spec push_message(pid(), #message{})-> ok.
push_message(Pid, Message) ->
    gen_server:cast(Pid, {push, Message}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec get_count() -> {ok, integer()}.
get_count() ->
    {ok, ets:info(producer, size)}.

-spec subscribe(pid(), code()) ->
                       ok | {error, permission_denied}.
subscribe(Pid, Channel_code)->
    gen_server:call(Pid, {subscribe, Channel_code}).

unsubscribe(Pid, Channel_code)->
    gen_server:call(Pid, {unsubscribe, Channel_code}).

-spec publish(pid(), #message{}) -> ok
                                        | {error, producer_not_found}
                                        | {error, permission_denied}.
publish(Pid, Message)->
    gen_server:call(Pid, {publish, Message}).

-spec get_subscribtions(pid()) -> {ok, dict:dict(binary(), pid())}.
get_subscribtions(Pid) ->
    gen_server:call(Pid, get_subscribtions).

-spec add_message_handler(pid(), pid()) -> ok.
add_message_handler(Pid, Handler_pid) ->
    gen_server:call(Pid, {add_message_handler, Handler_pid}).

-spec remove_message_handler(pid(), pid()) -> ok.
remove_message_handler(Pid,Handler_pid) ->
    gen_server:call(Pid, {remove_message_handler, Handler_pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
init([Code, Meta]) ->
    Self = self(),
    lager:info("CODE=~p~n", [Code]),
    lager:info("SELF=~p~n", [Self]),
    lager:info("VAL=~p~n", [pc_global:get_or_register_producer(Code)]),
    lager:info("VAL2=~p~n", [pc_global:get_or_register_producer(Code)]),
    case pc_global:get_or_register_producer(Code) of
	Self ->
            % initialize permission backend after initialization
            {Module, Args} = pc_permission_backend:get_backend(),
            case Module:start_link(Code, Args, Meta) of
                {ok, Pid} -> {ok,
                              #state{
                                 code=Code,
                                 channels=dict:new(),
                                 messages=queue:new(),
                                 max_message_count=20,
                                 current_message_count=0,
                                 handlers=[],
                                 permission_module=Module,
                                 permission_backend=Pid},
                              ?TIMEOUT};
                {error, Reason} ->
                    lager:info("Start link returned an error ~p~n", [Reason]),
                    {error, Reason}
            end;
	Pid when is_pid(Pid) -> 
	    {error, {already_started, Pid}}
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_code, _From, #state{code=Code}=State) ->
    Reply = {ok, Code},
    {reply, Reply, State, ?TIMEOUT};

handle_call({publish,
             #message{producer_code=Producer_code,
                      channel_code=Channel_code}=Message},
             _From,
            #state{code=Producer_code,
                   permission_module=Perm_module,
                   permission_backend=Perm_backend}=State) ->
    case get_or_create_channel(Channel_code, State) of
        {ok, Channel_pid} ->
            case Perm_module:has_permission(Perm_backend, publish, Channel_code) of
                true ->
                    pc_channel:publish(Channel_pid, Message),
                    {reply, ok, State, ?TIMEOUT};
                false ->
                    {reply, {error, permission_denied}, State, ?TIMEOUT}
            end;
        {error, permission_denied} ->
            {reply, {error, permission_denied}, State, ?TIMEOUT}
    end;

handle_call({subscribe, Channel_code}, _From,
	    #state{code=Code,
		   channels=Channels_dict,
                   permission_module=Perm_module,
                   permission_backend=Perm_backend}=State) ->
    case get_or_create_channel(Channel_code, State) of
        {ok, Channel_pid} ->
            Reply = ok,
            State2 = State,
            %% if value is already exist in the dictionary log a warning
            case dict:is_key(Channel_code, Channels_dict) of
                true ->
                    {reply, Reply, State, ?TIMEOUT};
                false ->
                    case Perm_module:has_permission(
                           Perm_backend, subscribe, Channel_code) of
                        true ->
                            ok = pc_channel:add_producer(Channel_pid, self(), Code),
                            {reply,
                             Reply,
                             State2#state{channels=dict:store(Channel_code,
                                                              Channel_pid,
                                                              Channels_dict)},
                             ?TIMEOUT};
                        false ->
                            {reply, {error, permission_denied}, State, ?TIMEOUT}
                    end
            end;
        {error, permission_denied} ->
            {reply, {error, permission_denied}, State}
    end;

handle_call({unsubscribe, Channel_code}, _From,
	    #state{channels=Channels_dict,
		   code=Code}=State) ->
    Reply = ok,
    %% if value is already exist in the dictionary log a warning
    case dict:find(Channel_code, Channels_dict) of
	{ok, Channel_pid} ->
	    Channels_dict2 = dict:erase(Channel_code, Channels_dict),
	    ok = pc_channel:remove_producer(Channel_pid, Code);
	error ->
	    Channels_dict2 = Channels_dict
    end,
    {reply, Reply, State#state{channels=Channels_dict2}, ?TIMEOUT};


handle_call({add_message_handler, Handler_pid}, _From,
	    #state{handlers=Handler_list}=State) ->
    case lists:member(Handler_pid, Handler_list) of
	true -> New_handler_list = Handler_list;
	false -> New_handler_list = [Handler_pid | Handler_list]
    end,
    Reply = ok,
    {reply, Reply, State#state{handlers=New_handler_list}, ?TIMEOUT};

handle_call({remove_message_handler, Handler_pid}, _From,
	    #state{handlers=Handler_list}=State) ->
    Reply = ok,
    {reply, Reply, State#state{handlers=lists:delete(Handler_pid, Handler_list)}, ?TIMEOUT};


%% Gets all messages for this user and returns them
handle_call(get_messages, _From, #state{messages=Messages_queue}=State) ->
    Reply = {ok, queue:to_list(Messages_queue)},
    {reply, Reply, State#state{messages=queue:new(),
                               current_message_count=0}, ?TIMEOUT};

handle_call(get_subscribtions, _From, #state{channels=Channels_dict}=State)->
    Reply = {ok, dict:fetch_keys(Channels_dict)},
    {reply,Reply, State, ?TIMEOUT};


handle_call({get_producers,
             Channel_code}, _From, State)->
    case get_or_create_channel(Channel_code, State) of
        {ok, Channel_pid} ->
            {ok, Consumer_list} = pc_channel:get_producers(Channel_pid),
            {reply, {ok, Consumer_list}, State, ?TIMEOUT};
        {error, permission_denied} ->
            {reply, {error, permission_denied}, State, ?TIMEOUT}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% If there is no handler, just put the message to cache
handle_cast({push, Message},
	    #state{messages=Message_queue,
                   max_message_count=Max_message_count,
                   current_message_count=Current_message_count,
                   handlers=[]}=State) ->
    if
        Max_message_count =< Current_message_count ->
            Message_queue1 = queue:drop(queue:in(Message,Message_queue)),
            Current_message_count1 = Current_message_count;
        Max_message_count > Current_message_count ->
            Message_queue1 = queue:in(Message,Message_queue),
            Current_message_count1 = Current_message_count + 1
    end,
    
    {noreply, State#state{messages=Message_queue1,
                          current_message_count=Current_message_count1},
     ?TIMEOUT};

handle_cast({push, Message},
	    #state{handlers=Handler_list}=State) ->
    Alive_handler_list = lists:filter(fun is_process_alive/1, Handler_list),
    New_state = State#state{handlers=Alive_handler_list},
    case Alive_handler_list of
	[] ->
	    lager:info("All hadlers are dead, Switch to buffer mode"),
	    handle_cast({push, Message}, New_state);
	_ ->
            lists:foreach(fun(Pid)->
				   Pid ! Message
                          end, Handler_list),
	     {noreply, New_state, ?TIMEOUT}
	end;

handle_cast(stop, State) ->
    {stop, normal, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{code=Code,
                            handlers=Handler_list}=State)->
    lager:info("Consumer ~p has timeout", [Code]),
    Alive_handler_list = lists:filter(fun is_process_alive/1, Handler_list),
    case Alive_handler_list of
	[] ->
	    lager:info("~p - All handlers are dead. Consumer is dying.", [Code]),
            {stop, normal , State};
	_ ->
            lager:info("~p - There is still alive handlers. Consumer will stay alive",
                       [Code]),
            {noreply, State, ?TIMEOUT}
	end;

handle_info(Info, #state{code=Code}=State) ->
    lager:warning("Unhandled info message in producer ~p (~p)", [Code, Info]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, #state{code=Code,
                         channels=Channel_dict}=_State) ->
    dict:fold(fun(_Channel_code, Pid, ok)->
                      pc_channel:remove_producer(Pid, Code),
                      ok end, ok, Channel_dict),
    lager:info("Terminate producer ~p (~p) ", [Code, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_or_create_channel(Channel_code,
                      #state{channels=Channels,
                             permission_module=Perm_module,
                             permission_backend=Perm_backend})->

    case dict:find(Channel_code, Channels) of
	{ok, Channel_pid} ->
            % Channel in channels list
            {ok, Channel_pid};
	error ->
            case pc_global:get_channel(Channel_code) of
                undefined ->
                    % Channel is not created before. Create Channel
                    case Perm_module:has_permission(Perm_backend, create, Channel_code) of
                        true -> pc_channel_sup:start_child(Channel_code);
                        false -> {error, permission_denied}
                    end;
                Pid when is_pid(Pid) ->
                    % Channel is already created.
                    {ok, Pid}
            end
    end.

