
%%%
%%%             Data Server
%%%
%%% @author Joseph Lenton
%%% @doc
%%% The Data Server is a special type of server that holds data whilst offering
%%% call back functionality on messages and requests. The result of the callback
%%% depicts the new data of the server.
%%% 
%%% When created two functions are required:
%%%     on_message( Message, Parameters, State ) -> State
%%%         Deals with the message and is passed in the server state. It returns
%%%         the servers new state when it ends.
%%%     on_request( Message, Parameters, State ) -> { State, ReturnVal }
%%%         Same as OnMessage, only this needs to return a tuple containing the
%%%         servers new state and the value to return to the initial sender.
%%% 
%%% @author Joseph Lenton
%%% 

-module( data_server ).
-export([
    new/3, new/4,
    new_in_pid/3, new_in_pid/4,
    send/2, send/3,
    request/2, request/3,
    request_timeout/3, request_timeout/4,
    send_request/2, send_request/3,
    receive_request/1, receive_request/2,
    set_message_forward/2
]).

-define(DATA_SERVER_MESSAGE_SEND, data_server_message_send).
-define(DATA_SERVER_MESSAGE_REQUEST, data_server_message_request).
-define(DATA_SERVER_MESSAGE_REQUEST_RESPONSE, data_server_message_request_response).
-define(DATA_SERVER_MESSAGE_DELAY_REQUEST, data_server_message_delay_request).
-define(DATA_SERVER_MESSAGE_DELAY_REQUEST_RESPONSE, data_server_message_delay_request_response).
-define(DATA_SERVER_SET_MESSAGE_FORWARD, data_server_set_message_forward).
-define(DATA_SERVER_SET_MESSAGE_FORWARD_RESPONSE, data_server_set_message_forward_response).

-define(DATA_SERVER_NO_MAIL_FORWARD, none).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      new
%%
%% Creates a new server. The OnSend and OnRequest functions will be called when
%% messages are sent and requested to this server. The Data is the servers
%% initial data.
%% 
%% The two versions of new allow the user to spawn a server that is, or is not,
%% linked to the current process. The default is false.
%% 
%% @spec new( OnMessage, OnRequest, StartState::term() ) -> PID::pid()
%%      OnMessage = ( Params::term(), State::term() ) -> term()
%%      OnRequest = ( Params::term(), State::term() ) -> { term(), term() }
new(OnMessage, OnRequest, State) -> new(OnMessage, OnRequest, State, true).

%%      new
%%
%% Same as the other new only this allows you to state to link or not link this
%% data_server to the currently executing process.
%% 
%% @spec new( OnMessage, OnRequest, StartState::term(), SpawnLink::boolean() ) -> PID::pid()
%%      OnMessage = ( Params::term(), State::term() ) -> term()
%%      OnRequest = ( Params::term(), State::term() ) -> { term(), term() }
new(OnMessage, OnRequest, State, SpawnLink ) -> new_in_pid( OnMessage, OnRequest, fun() -> State end, SpawnLink ).

%%      new_in_pid
%%
%% The same as new, only the given state value is treated as a function that
%% will generate the start state for this server. This allows the state to be
%% created in the same process as the server, which certain libraries require.
%% 
%% @spec new_in_pid( OnMessage, OnRequest, StateFunc ) -> PID::pid()
%%      OnMessage = ( Params::term(), State::term() ) -> term()
%%      OnRequest = ( Params::term(), State::term() ) -> { term(), term() }
%%      StateFunc = () -> term()
new_in_pid(OnMessage, OnRequest, StateFunc) -> new_in_pid(OnMessage, OnRequest, StateFunc, true).
new_in_pid(OnMessage, OnRequest, StateFunc, true ) ->
    spawn_link( new_loop_fun(OnMessage, OnRequest, StateFunc) );
new_in_pid(OnMessage, OnRequest, StateFunc, false) ->
    spawn     ( new_loop_fun(OnMessage, OnRequest, StateFunc) ).

%%      new_loop_fun
%%
%% @doc Creates and returns a fun which contains the standard loop for this process.
%% @spec new_loop_fun(OnMessage, OnRequest, StateFunc) -> LoopFun
%%      OnMessage = ( Params::term(), State::term() ) -> term()
%%      OnRequest = ( Params::term(), State::term() ) -> { term(), term() }
%%      StateFunc = () -> term()
%%      LoopFun   = () -> term()
new_loop_fun(OnMessage, OnRequest, StateFunc) ->
    fun() ->
            process_flag( trap_exit, true ),
            loop( OnMessage, OnRequest, false, ?DATA_SERVER_NO_MAIL_FORWARD, StateFunc() )
    end.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Message Handling
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      loop
%% 
%% @doc Deals with each type of message in turn.
%% @spec loop( OnMessage, OnRequest, IsMailForwarded::boolean(), MailDestination::pid(), State::term() ) -> term()
%%      OnMessage = ( Params::term(), State::term() ) -> term()
%%      OnRequest = ( Params::term(), State::term() ) -> { term(), term() }
loop(OnMessage, OnRequest, IsMailForwarded, MailDestination, State) ->
    if
        IsMailForwarded ->
            receive
                {?DATA_SERVER_MESSAGE_SEND, Message} ->
                    NewState = handle_send(Message, OnMessage, State),
                    loop( OnMessage, OnRequest, IsMailForwarded, MailDestination, NewState );
                {?DATA_SERVER_MESSAGE_REQUEST, Message} ->
                    NewState = handle_request(Message, OnRequest, State),
                    loop( OnMessage, OnRequest, IsMailForwarded, MailDestination, NewState );
                {?DATA_SERVER_MESSAGE_DELAY_REQUEST, Message} ->
                    NewState = handle_delay_request(Message, OnRequest, State),
                    loop( OnMessage, OnRequest, IsMailForwarded, MailDestination, NewState );
                {?DATA_SERVER_SET_MESSAGE_FORWARD, NewMailDestination, Sender} ->
                    Sender ! ?DATA_SERVER_SET_MESSAGE_FORWARD_RESPONSE,
                    NewIsMailForwarded = not (NewMailDestination =:= ?DATA_SERVER_NO_MAIL_FORWARD),
                    loop( OnMessage, OnRequest, NewIsMailForwarded, NewMailDestination, State );
                Message ->
                    MailDestination ! Message,
                    loop( OnMessage, OnRequest, IsMailForwarded, MailDestination, State )
            end;
        true ->
            receive
                {?DATA_SERVER_MESSAGE_SEND, Message} ->
                    NewState = handle_send(Message, OnMessage, State),
                    loop( OnMessage, OnRequest, IsMailForwarded, MailDestination, NewState );
                {?DATA_SERVER_MESSAGE_REQUEST, Message} ->
                    NewState = handle_request(Message, OnRequest, State),
                    loop( OnMessage, OnRequest, IsMailForwarded, MailDestination, NewState );
                {?DATA_SERVER_MESSAGE_DELAY_REQUEST, Message} ->
                    NewState = handle_delay_request(Message, OnRequest, State),
                    loop( OnMessage, OnRequest, IsMailForwarded, MailDestination, NewState );
                {?DATA_SERVER_SET_MESSAGE_FORWARD, NewMailDestination, Sender} ->
                    Sender ! ?DATA_SERVER_SET_MESSAGE_FORWARD_RESPONSE,
                    NewIsMailForwarded = not (NewMailDestination =:= ?DATA_SERVER_NO_MAIL_FORWARD),
                    loop( OnMessage, OnRequest, NewIsMailForwarded, NewMailDestination, State )
            end
    end.

handle_send({Message, Params}, OnMessage, State) -> OnMessage( Message, Params, State ).
handle_request( {Message, Params, Sender}, OnRequest, State ) ->
    {NextState, ReturnVal} = OnRequest( Message, Params, State ),
    Sender ! {?DATA_SERVER_MESSAGE_REQUEST_RESPONSE, Message, ReturnVal},
    NextState.
handle_delay_request( {Message, Params, Sender, Token}, OnRequest, State ) ->
    {NextState, ReturnVal} = OnRequest( Message, Params, State ),
    Sender ! {?DATA_SERVER_MESSAGE_REQUEST_RESPONSE, Token, ReturnVal},
    NextState.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Functions
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      send
%%
%% @doc Sends the message to the given data_server. It's parameters is the atom ok.
%% The OnMessage fun stored by the data_server will then handle the message.
%% 
%% @spec send( Server::data_server(), Message::atom() ) -> ok
send( Server, Message ) ->
    send( Server, Message, ok ).

%%      send
%%
%% @doc Sends the given message, with the given parameters, to the given server.
%% The OnMessage fun stored by the data_server will then handle the message with
%% the given parameters.
%%
%% @spec send( Server::data_server(), Message::atom(), Params::term() ) -> ok
send(Server, Message, Params) ->
    Server ! {?DATA_SERVER_MESSAGE_SEND, {Message, Params}}.

%%      request
%%
%% @doc Sends the message to the server and then returns the value returned from the request.
%% The ok atom will be sent as the default parameters.
%%
%% @spec request( Server::data_server(), Message::atom() ) -> term()
request(Server, Message) -> request( Server, Message, ok ).

%%      request
%%
%% @doc Sends the message to the server and then returns the value returned from the request.
%% The Message and the Params will be passed into the data_servers OnRequest fun.
%% This fun defines the value to return.
%%
%% @spec request( Server::data_server(), Message::atom(), Params::term() ) -> term()
request(Server, Message, Params) ->
    Server ! {?DATA_SERVER_MESSAGE_REQUEST, {Message, Params, self()}},
    receive
        {?DATA_SERVER_MESSAGE_REQUEST_RESPONSE, _Msg=Message, ReturnVal} ->
            ReturnVal
    end.

%%      request_timeout
%%
%% @doc A request reply, but with a timeout value.
%% If a response is not received within the given amount of time then failure is
%% presumed and the atom timeout will be returned. Otherwise it will be the
%% return value of the request.
%%
%% @spec request_timeout( Server::data_server(), Message::atom(), Timeout::integer() ) -> timeout | term()
request_timeout(Server, Message, Timeout) -> request_timeout( Server, Message, ok, Timeout ).

%%      request_timeout
%%
%% @doc The same as the other request_timeout function, only this also allows a request parameter.
%% @spec request_timeout( Server::data_server(), Message::atom(), Params::term(), Timeout::integer() ) -> timeout | term()
request_timeout(Server, Message, Params, Timeout) ->
    Server ! {?DATA_SERVER_MESSAGE_REQUEST, {Message, Params, self()}},
    receive
        {?DATA_SERVER_MESSAGE_REQUEST_RESPONSE, _Msg=Message, ReturnVal} ->
            ReturnVal
    after
        Timeout ->
            timeout
    end.

%%      send_request
%%
%% @doc This is the same as request, except that it does not block and wait for a reply.
%% Instead you must use collect_request at a later date to receive the reply. To
%% receive the reply you will also need to use the TimeStamp returned by this
%% send_request function.
%%
%% @spec send_request( Server::data_server(), Message::atom() ) -> TimeStamp::integer()
send_request(Server, Message) -> send_request(Server, Message, ok).

%%      send_request
%% 
%% @doc A request is sent but the function returns directly.
%% The return value should be retrieved later using the receive_request function.
%%
%% @spec send_request( Server::data_server(), Message::atom(), Params::term() ) -> TimeStamp::integer()
send_request(Server, Message, Params) ->
    Token = util:get_time(),
    Server ! {?DATA_SERVER_MESSAGE_DELAY_REQUEST, {Message, Params, self(), Token}},
    Token.

%%      collect_request
%%
%% @doc Receives the return value from a previous sent request to a data server.
%% The TimeStamp given is a unique identifier to the request to receive. If the
%% request is never received then this will block indefinitely.
%%
%% The TimeStamp should be a value returned from the send_request function.
%%
%% @spec receive_request( TimeStamp::integer() ) -> term()
receive_request( TimeStamp ) ->
    receive
        {?DATA_SERVER_MESSAGE_DELAY_REQUEST, _TStamp=TimeStamp, ReturnVal} ->
                ReturnVal
    end.

%%      receive_request
%%
%% @doc Same as the other collect_request function only this has a time out value.
%% @spec receive_request( TimeStamp::integer(), TimeOut::integer() ) -> term()
receive_request(TimeStamp, TimeOut) ->
    receive
        {?DATA_SERVER_MESSAGE_DELAY_REQUEST, _TStamp=TimeStamp, ReturnVal} ->
                ReturnVal
    after TimeOut ->
            ok
    end.

%%      set_message_forward
%%
%% @doc Sets where to forward messages on to.
%% This is to allow servers to spawn child processes where work is performed,
%% that look exactly like the server who spawned them, whilst the server can
%% deal with more incomming requests.
%%
%% Setting the destination to none will disable mail forwarding.
%% 
%% @spec set_message_forward( Server::server(), Destination::pid() | none ) -> ok
set_message_forward( Server, Destination ) ->
    Server ! {?DATA_SERVER_SET_MESSAGE_FORWARD, Destination, self()},
    receive
        ?DATA_SERVER_SET_MESSAGE_FORWARD_RESPONSE -> ok
    end.
