
%%%
%%%             function_server
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This is an extension on the data_server. It differes in that functions can
%%% be registered onto this server allowing them to be automatically looked up
%%% as new messages arrive.
%%%
%%% Two custom functions can be supplied on creation for instances when no
%%% registered message can be found. By default an error is raised when no
%%% message is retrieved.
%%% 
%%% The no_message and no_request function follow the same signature as
%%% on_message and on_request of the data_server:
%%%     fun( Message, Parameters, State )
%%%
%%% All of the registered on_message and on_request functions must have the
%%% signatures:
%%%     on_message( Parameters, State ) -> State
%%%         Deals with the message and is passed in the server state. It returns
%%%         the servers new state when it ends.
%%%     on_request( Parameters, State ) -> { State, ReturnVal }
%%%         Same as OnMessage, only this needs to return a tuple containing the
%%%         servers new state and the value to return to the initial sender.
%%% 
%%% @author Joseph Lenton
%%% 

-module( function_server ).
-author("Joseph Lenton").

-export([
        new/0, new/1, new/3, new/5,
        new_spawn/3, new_spawn/5,
        set_message_funcs/2,
        set_request_funcs/2,
        set_message_func/3,
        set_request_func/3
]).

% functions of the super data_server module
-export([
        send/2, send/3,
        request/2, request/3,
        send_request/2, send_request/3,
        receive_request/1, receive_request/2,
        receive_timeout/3, receive_timeout/4
]).

-define( REGISTERED_SERVER_SET_MESSAGE_FUNC,  registered_server_set_message_func ).
-define( REGISTERED_SERVER_SET_REQUEST_FUNC,  registered_server_set_request_func ).
-define( REGISTERED_SERVER_ADD_MESSAGE_FUNCS, registered_server_add_message_funcs ).
-define( REGISTERED_SERVER_ADD_REQUEST_FUNCS, registered_server_add_request_funcs ).

-record( registered_server_state, {
        state,
        on_messages,
        on_requests,
        no_message,
        no_request
}).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          new
%%
%% @doc Creates a new server with 'ok' as it's initial state.
%%
%% @spec new() -> PID
new()       -> new( ok ).

%%          new
%%
%% @doc Creates a new state that holds the given state.
%% This state is passed into any of it's registered functions for when they are
%% called.
%% 
%% @spec new( State ) -> PID
new(StartState)  ->
    new( StartState, [], [] ).

new(StartState, MessageFuncs, RequestFuncs) ->
    OnError = new_on_error(),
    new( StartState, MessageFuncs, RequestFuncs, OnError, OnError ).

%%          new
%%
%% This server will call the given NoMessage and NoRequest functions for
%% deciding on how to act when no message or request function is found to deal
%% with a message.
%%
new(StartState, MessageFuncs, RequestFuncs, NoMessage, NoRequest) ->
    data_server:new(
            fun(Message, Params, State) -> on_message(Message, Params, State) end,
            fun(Message, Params, State) -> on_request(Message, Params, State) end,
            new_state( StartState, MessageFuncs, RequestFuncs, NoMessage, NoRequest )
    ).

new_spawn(StartState, MessageFuncs, RequestFuncs) ->
    OnError = new_on_error(),
    new_spawn( StartState, MessageFuncs, RequestFuncs, OnError, OnError ).
new_spawn(StartState, MessageFuncs, RequestFuncs, NoMessage, NoRequest) ->
    data_server:new_in_pid(
            fun(Message, Params, State) -> on_message(Message, Params, State) end,
            fun(Message, Params, State) -> on_request(Message, Params, State) end,
            fun() ->
                new_state( StartState(), MessageFuncs, RequestFuncs, NoMessage, NoRequest )
            end
    ).

%%          new_state
%%
%% @doc Creates a new empty internal state for the fucntion_server.
%% Note that this record is the internal state, not for use outside of the
%% module.
new_state(State, MessageFuncs, RequestFuncs, NoMessage, NoRequest) ->
    NewFuncState = #registered_server_state {
            state = State,
            on_messages = dict:new(),
            on_requests = dict:new(),
            no_message = NoMessage,
            no_request = NoRequest
    },
    store_message_funcs(
            store_request_funcs( NewFuncState, RequestFuncs ),
            MessageFuncs ).

%%          new_on_error
%%
%% This creates the default function that is called when a message or request
%% cannot be dealt with. This function will simply raise an erlang error.
%%
%% @spec new_on_error() -> (Message::atom(), Params::term(), State::term()) -> term()
new_on_error() ->
    fun(Message, Params, State) ->
            erlang:error("No functions registered for message.", [ Message, Params, State ])
    end.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Message Handling
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

store_message_funcs( State, Funcs ) ->    
    OnMessages = State#registered_server_state.on_messages,
    State#registered_server_state{ on_messages = store_funcs(OnMessages, Funcs) }.

store_request_funcs( State, Funcs ) ->
    OnRequests = State#registered_server_state.on_requests,
    State#registered_server_state{ on_requests = store_funcs(OnRequests, Funcs) }.

%%          store_funcs
%%
%% A helper function for storing the given list of Funcs (which should be tuples
%% of: {Message, Func}) into the given dict. A new Dict is returned containing
%% all of the registered functions.
%%
%% @spec store_funcs(Dict, [ {Message, Func} ]) -> Funcs::dict()
%%      Func = ( Params::term(), State::term() ) -> term()
store_funcs(Dict, []) -> Dict;
store_funcs(Dict, [ {Message, Func} | Funcs ] ) ->
    store_funcs( dict:store(Message, Func, Dict), Funcs ).

%%          on_message
%%
%% Deals with any function_server specific messages, or failing that will find
%% a registered function for the given message. If not found then the NoMessage
%% function will be called instead.
%% 
%% @spec on_message(Message::atom(), Params::term(), CurrentState::function_server()) -> NewState::function_server()
on_message(?REGISTERED_SERVER_ADD_MESSAGE_FUNCS, Funcs, State) ->
    store_message_funcs( State, Funcs );
on_message(?REGISTERED_SERVER_ADD_REQUEST_FUNCS, Funcs, State) ->
    store_request_funcs( State, Funcs );
on_message(?REGISTERED_SERVER_SET_MESSAGE_FUNC, {Message, Func}, State) ->
    OnMessages = State#registered_server_state.on_messages,
    State#registered_server_state{ on_messages = dict:store(Message, Func, OnMessages) };
on_message(?REGISTERED_SERVER_SET_REQUEST_FUNC, {Message, Func}, State) ->
    OnRequests = State#registered_server_state.on_requests,
    State#registered_server_state{ on_requests = dict:store(Message, Func, OnRequests) };
on_message(Message, Params, State) ->
    case dict:find( Message, State#registered_server_state.on_messages ) of
            {ok, Func} ->
                    State#registered_server_state{ state=Func( Params, State#registered_server_state.state ) };
            error ->
                    ErrorFunc = State#registered_server_state.no_message,
                    State#registered_server_state{ state=ErrorFunc( Message, Params, State#registered_server_state.state ) }
    end.

%%          on_request
%% 
%% Very similar to the on_message function, this will deal with any function
%% server specific details first and failing that will call any registered
%% request functions in the function server.
%% 
%% @spec on_request(Message, Params, State::function_server()) -> {NewState::function_server(), ReturnVal::term()}
on_request(Message, Params, State) ->
    case dict:find( Message, State#registered_server_state.on_requests ) of
            {ok, Func} ->
                { NewInnerState, ReturnVal } = Func( Params, State#registered_server_state.state ),
                { State#registered_server_state{ state = NewInnerState }, ReturnVal };
            error ->
                ErrorFunc = State#registered_server_state.no_request,
                { NewInnerState, ReturnVal } = ErrorFunc( Message, Params, State#registered_server_state.state ),
                { State#registered_server_state{ state = NewInnerState }, ReturnVal }
    end.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      set_message_funcs
%%
%% @doc This registers a whole list of functions to this server.
%% Each element in the list should be in the format: {Message, Func}.
%% 
%% @spec set_message_funcs( Server::pid, [{Message::atom(), Fun}] ) -> ok
%%      Fun = ( Params::term(), State::term() ) -> term()
set_message_funcs( Server, Fun ) ->
    data_server:send(Server, ?REGISTERED_SERVER_ADD_MESSAGE_FUNCS, Fun).

%%      set_request_funcs
%%
%% @doc Same as set_message_funcs only this is for the on_request functions.
%% @spec set_request_funcs( Server::pid, [{Message, Fun}] ) -> ok
%%      Fun = ( Params::term(), State::term() ) -> { term(), term() }
set_request_funcs( Server, Fun ) ->
    data_server:send(Server, ?REGISTERED_SERVER_ADD_REQUEST_FUNCS, Fun).

%%      set_message_func
%%
%% @doc Registeres the given function onto the given server object.
%% @spec set_message_func( Server::pid, Message::atom(), Fun ) -> ok
%%      Fun = ( Params::term(), State::term() ) -> term()
set_message_func( Server, Message, Func ) ->
    data_server:send(Server, ?REGISTERED_SERVER_SET_MESSAGE_FUNC, {Message, Func}).

%%      set_request_func
%%
%% @doc Registeres the given function onto the given server object.
%% @spec set_request_func( Server::pid, Message::atom(), Fun ) -> ok
%%      Fun = ( Params::term(), State::term() ) -> { term(), term() }
set_request_func( Server, Message, Func ) ->
    data_server:send(Server, ?REGISTERED_SERVER_SET_REQUEST_FUNC, {Message, Func}).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API - Super Functions
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      send
%%
%% @equiv data_server:send( Server, Message )
send( Server, Message ) -> data_server:send( Server, Message ).

%%      send
%%
%% @equiv data_server:send( Server, Message, Params)
send(Server, Message, Params) -> data_server:send( Server, Message, Params ).

%%      request
%%
%% @equiv data_server:request( Server, Message )
request(Server, Message) -> data_server:request( Server, Message ).

%%      request
%%
%% @equiv data_server:request( Server, Message, Params )
request(Server, Message, Params) ->  data_server:request( Server, Message, Params ).

%%      send_request
%%
%% @equiv data_server:send_request( Server, Message )
send_request(Server, Message) ->  data_server:send_request( Server, Message ).

%%      send_request
%%
%% @equiv data_server:send_request( Server, Message, Params )
send_request(Server, Message, Params) -> data_server:send_request( Server, Message, Params ).

%%      receive_request
%%
%% @equiv data_server:receive_request( Token )
receive_request(Token) -> data_server:receive_request( Token ).

%%      receive_request
%%
%% @equiv data_server:receive_request( TimeStamp, TimeOut )
receive_request(TimeStamp, TimeOut) -> data_server:receive_request( TimeStamp, TimeOut ).

%%      receive_timeout
%%
%% @equiv data_server:receive_timeout( Server, Message, Timeout )
receive_timeout(Server, Message, Timeout) -> data_server:receive_timeout( Server, Message, Timeout ).

%%      request_timeout
%%
%% @equiv data_server:receive_timeout( Server, Message, Params, Timeout )
receive_timeout(Server, Message, Params, Timeout) -> data_server:receive_timeout( Server, Message, Params, Timeout ).