%%% 
%%%             Actor
%%%
%%% @author Joseph Lenton
%%% @doc
%%% The Actor module defines how to create and interact with Actors in the world.
%%% 
%%% An Actor is made up of three aspects:<br />
%%%     = State - an instance of actor_state<br />
%%%     = Act - A callback function defining how to update this Actors state.<br />
%%%     = Paint - A callback function defining how to paint.<br />
%%%
%%%     <em>Actor State</em><br />
%%% The ActorState should be an instance made from the actor_state module (or a
%%% conceptual sub-class of this). Custom properties can be added to the
%%% actor_state when it is setup.
%%%
%%%     <em>Act( State::actor_state(), Parent::pid() ) -> NewState::actor_state()</em><br />
%%% The Act function defines how to change or update the Actors actor_state on
%%% each frame. The result of this function is the Actors state for the next
%%% frame. The signature of the Act function must be in the format stated above.
%%% The parent is PID to the World that this Actor is in.
%%%
%%%     <em>Paint( State::actor_state(), G::graphics() ) -> ok</em><br />
%%% The Paint function defines how to paint the Actors actor_state to the
%%% screen. The result of this callback function is ignored. The function must
%%% be in the signature defined above. The given graphics object, G, can be
%%% passed into the functions in the graphics module to perform drawing.
%%%

-module(actor).

-author("Joseph Lenton").

-include("actor.hrl").
-include("actor_state.hrl").
-include("controls_handler.hrl").

-export([ new/2, new/3 ]).
-export([
    act/2,
    paint/2,
    send_act/2, receive_act/0
]).
-export([
    get_act/1,
    get_paint/1,
    get_state/1,
    get_name/1,
    get_controls/1
]).
-export([ compare/2 ]).

-define( NO_PARENT          , actor_no_parent ).
-define( PAINT_TIMEOUT      , 100             ). % in milliseconds

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      new
%% 
%% @doc Creates a new Actor with a blank actor_state.
%% It will use the act and paint callback functions given to update and paint
%% it's default state.
%% @spec new( Act::ActFunc, Paint::PaintFunc ) -> NewActor::actor()
%%          ActFunc   = ( State::actor_state(), Parent::pid() ) -> actor_state()
%%          PaintFunc = ( State::actor_state(), G::graphics() ) -> term()
new(Act, Paint) when is_function(Act) and is_function(Paint) ->
    new( Act, Paint, actor_state:new() ).

%%      new
%%
%% @doc Creates a new Actor which uses the given state as it's initial actor_state.
%% It will use the act and paint callback functions given to update and paint.
%% @spec new( Act::ActFunc, Paint::PaintFunc, StartState::actor_state() ) -> NewActor::actor()
%%          ActFunc   = ( State::actor_state(), Parent::pid() ) -> actor_state()
%%          PaintFunc = ( State::actor_state(), G::graphics() ) -> term()
new(Act, Paint, StartState) when is_function(Act) and is_function(Paint) ->
    function_server:new_spawn(
            fun() ->
                #actor_server_state{
                        act         = Act,
                        paint       = Paint,
                        timestamp   = util:get_time(),
                        state       = actor_state:set( StartState, ?ACTOR_STATE_ACTOR, self() ),
                        parent      = ?NO_PARENT
                }
            end,
            [
                    { ?ACTOR_MESSAGE_ACT,
                            fun( {Sender, Parent}, State ) ->
                                Self = self(),
                                spawn( fun() ->
                                        process_flag( trap_exit, true ),
                                        data_server:set_message_forward( Self, self() ),

                                        ActFunc = State#actor_server_state.act,
                                        ActorState = State#actor_server_state.state,

                                        % We set the actor to this actor just incase it was lost at some point.
                                        % This could happen if an entirely new actor_state is returned.
                                        NewActorState = actor_state:set (
                                                ActFunc( ActorState, Parent ),
                                                ?ACTOR_STATE_ACTOR,
                                                Self ),
                                        data_server:set_message_forward( Self, none ),

                                        data_server:send( Self, ?ACTOR_MESSAGE_POST_ACT, NewActorState ),
                                        Sender ! ?ACTOR_MESSAGE_ACT_RESPONSE,
                                        forward_messages( Self )
                                end ),
                                
                                State#actor_server_state{ parent = Parent }
                            end },
                    { ?ACTOR_MESSAGE_POST_ACT,
                            fun( ActorState, State ) ->
                                State#actor_server_state{ state = ActorState }
                            end },
                    { ?ACTOR_ON_END_OF_MAINLOOP,
                            fun( _, State ) -> State end },
                    { ?CONTROLS_HANDLER_GET_CONTROLS,
                            fun( Sender, State ) ->
                                case State#actor_server_state.parent of
                                    ?NO_PARENT -> Sender ! { ?CONTROLS_HANDLER_GET_CONTROLS_RESPONSE, nil };
                                    Parent     -> data_server:send( Parent, ?CONTROLS_HANDLER_GET_CONTROLS, Sender )
                                end,
                                State
                            end },
                    { ?ACTOR_MESSAGE_RUN_ON_REMOVE,
                            fun( _Sender, State ) ->
                                OnRemoveFun = actor_state:get_on_remove( State#actor_server_state.state ),
                                OnRemoveFun(),
                                State
                            end }
            ],
            [
                    { ?ACTOR_MESSAGE_GET_PAINT_AND_STATE,
                            fun( _, State ) ->
                                {
                                    State,
                                    { State#actor_server_state.paint, State#actor_server_state.state }
                                }
                            end },
                    { ?ACTOR_MESSAGE_GET_NAME,
                            fun( _, State ) ->
                                {
                                    State,
                                    actor_state:get_name( State#actor_server_state.state )
                                }
                            end },
                    { ?ACTOR_MESSAGE_GET_ACT_FUNC,
                            fun( _, State ) -> { State, State#actor_server_state.act       } end },
                    { ?ACTOR_MESSAGE_GET_PAINT_FUNC,
                            fun( _, State ) -> { State, State#actor_server_state.paint     } end },
                    { ?ACTOR_MESSAGE_GET_STATE,
                            fun( _, State ) -> { State, State#actor_server_state.state     } end },
                    { ?ACTOR_MESSAGE_GET_TIMESTAMP,
                            fun( _, State ) -> { State, State#actor_server_state.timestamp } end }
            ]).

%%          forward_messages
%%
%% @doc Sends all messages it can currently receive on to the destination.
%% It times out straight away, so only messages it has received now will be
%% forwarded.
%%
%% @spec forward_messages( Destination::pid() ) -> ok
forward_messages( Destination ) ->
    receive
        Message ->
            Destination ! Message,
            forward_messages( Destination )
    after
        1 ->
            ok
    end.

%%      get_timestamp
%%
%% @doc The timestamp of when this actor was created.
%% @spec get_timestamp( Actor::actor() ) -> Timestamp
get_timestamp(Actor)    -> data_server:request( Actor, ?ACTOR_MESSAGE_GET_TIMESTAMP  ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      act
%%
%% @doc Requests that the given actor acts and updates it's state, this blocks until complete.
%% @spec act( Actor::actor(), Parent::pid() ) -> ok
act(Actor, Parent) ->
    send_act( Actor, Parent ),
    receive_act().

%%      send_act
%%
%% @doc This sends a non-blocking act message to the given actor.
%% The parent given will be passed in as it's parent. When the Actor has
%% finished acting it will send a response back to this process. To check for
%% this message the accompanied receive_act function should be used.
%%
%% @spec send_act( Actor::actor(), Parent::pid() ) -> ok
send_act(Actor, Parent) ->
    data_server:send( Actor, ?ACTOR_MESSAGE_ACT, {self(), Parent} ).

%%      receive_act
%%
%% @doc Waits for a response message from any Actor that this process call act on.
%% This should not be called unless the send_act function was first called.
%% Otherwise this will just block forever.
%% 
%% @spec receive_act() -> ok
receive_act() ->
    receive
        ?ACTOR_MESSAGE_ACT_RESPONSE -> ok
    end.

%%      paint
%%
%% @doc Calls to paint this Actor.
%%
%% Typically you only call this if you yourself are currently painting and you
%% want to perform a special case where another Actor will paint on demand.
%%
%% This will block until it is complete.
%% 
%% Warning: This should only ever be called within another Actor's paint
%% function and the painting will occur in the same process. It should never be
%% sent off to be run in a seperate process.
%%
%% @spec paint( Actor::actor(), G::graphics() ) -> ok
paint(Actor, G) ->
    case data_server:request_timeout( Actor, ?ACTOR_MESSAGE_GET_PAINT_AND_STATE, ?PAINT_TIMEOUT ) of
          timeout              -> ok;
        { APaintFunc, AState } -> APaintFunc( AState, G )
    end.

%%      get_name
%% 
%% @doc Returns the name currently stored in this actors state.
%% This is mainly here as a helper method to avoid getting the whole state of an
%% actor, just for one value.
%%
%% @spec get_name( Actor::actor() ) -> Name::name()
get_name(Actor)         -> data_server:request( Actor, ?ACTOR_MESSAGE_GET_NAME       ).

%%      get_state
%%
%% @doc Gets the actor_state from the given Actor.
%% @spec get_state( Actor::actor() ) -> State::actor_state()
get_state(Actor)        -> data_server:request( Actor, ?ACTOR_MESSAGE_GET_STATE      ).

%%      get_act
%%
%% @doc Gets the act callback function from the given Actor.
%% @spec get_act( Actor::actor() ) -> Act::( State::actor_state(), Parent::pid() ) -> actor_state()
get_act(Actor)          -> data_server:request( Actor, ?ACTOR_MESSAGE_GET_ACT_FUNC   ).

%%      get_paint
%%
%% @doc Returns the paint callback function from the given Actor.
%% @spec get_paint( Actor::actor() ) -> Paint::( State::actor_state(), G::graphics() ) -> term()
get_paint(Actor)        -> data_server:request( Actor, ?ACTOR_MESSAGE_GET_PAINT_FUNC ).

%%      compare
%% 
%% @doc Compares the two given Actors and returns true or false to state if ActorA should come before or after ActorB.
%% @spec compare( ActorA::actor(), ActorB::actor() ) -> bool()
compare(ActorA, ActorB) -> get_timestamp(ActorA) < get_timestamp(ActorB).

%%      get_controls
%%
%% @doc Asks the given Actor to get it's controls. nil is returned if this Actor cannot find the display.
%% The Actor needs to be owned (either directly or indirectly) by a running
%% Mainloop object.
%% @spec get_controls( Actor::actor() ) -> ( Controls::controls() | nil )
get_controls( Actor ) ->
    data_server:send( Actor, ?CONTROLS_HANDLER_GET_CONTROLS, self() ),
    receive
        { ?CONTROLS_HANDLER_GET_CONTROLS_RESPONSE, Controls } -> Controls
    end.

