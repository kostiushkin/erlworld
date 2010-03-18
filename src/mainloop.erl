%%% 
%%%             Mainloop
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This is the mainloop structure in a game. It can be thought of as the
%%% binding between the applications display and the gameplay objects (the
%%% Actors). The Mainloop holds an Actor (which is typically a World) as the
%%% root of the game. The rest of the games contents should be placed within
%%% this.
%%% 
%%% The Mainloop also requires a Display to be placed into it for when it's run
%%% for it to draw the Actor it holds to, and to retrieve the controls from.
%%% 
%%% A Mainloop can be run or stopped with the corresponding functions. When
%%% stopped the Actor within it is removed.
%%% 

-module( mainloop ).
-author( "Joseph Lenton" ).

-include("actor.hrl").
-include("display.hrl").
-include("mainloop.hrl").
-include("controls_handler.hrl").

% constructors
-export([ new/1, new/2 ]).
% functions for starting/stopping
-export([
    run/2, stop/1,
    set_actor/2, get_actor/1,
    get_controls/1
]).

-record( mainloop_state, {
        act_timestamp,  % used for frame limiting code
%        display,        % The screen.
        actor,          % The actor this mainloop calls to update/paint on each loop
        next_actor,     % The actor to use on the next cycle of update/paint
        is_running,     % bool to state if this is currently acting or not
        frame_time,     % This is how long a frame is expected to take
        controls        % this is a snapshot of the last state of the controls
}).

% This is the amount of time that each loop should take. It is 1 second, in
% microseconds, divided by 65 frames. (limits to 65fps)
-define( ONE_SECOND        , 1000000 ).
-define( FRAME_WAIT_TIME   , 15384 ). % (1000000 div 65) -> 15384
-define( DEFAULT_FRAME_TIME, 60 ).
-define( NO_ACTOR          , nil ).

% messages for the mainloop sync
-define( SYNC_RECEIVE_ACTOR, sync_receive_actor ).
-define( SYNC_ACT_DONE     , sync_act_done      ).
-define( SYNC_PAINT_DONE   , sync_paint_done    ).

% common messages used by the sync, paint and act processes
-define( USE_PID    , use_pid_message     ).
-define( USE_ACTOR  , use_actor_message   ).
-define( PAINT_ACTOR, paint_actor_message ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%              new
%% 
%% @doc Creates and returns a new mainloop object which wraps the given display.
%% @spec new( Display::display() ) -> MainLoop::mainloop()
new( Display ) -> new( Display, ?DEFAULT_FRAME_TIME ).

new( Display, FrameRate ) ->
    % this is used for telling the mainloop to act and paint again after both have been run
    Sync = spawn(
            fun() ->
                receive
                    {?USE_PID, PID} ->
                        loop_sync( PID )
                end
            end
    ),
    
    % re-usable processes for painting and acting in parrallel
    Paint = spawn(
            fun() ->
                receive
                    {?USE_PID, PID} ->
                        loop_paint( Display, Sync, PID )
                end
            end
    ),
    Act = spawn(
            fun() ->
                receive
                    {?USE_PID, PID} ->
                        loop_act( Sync, PID )
                end
            end
    ),

    % make the mainloop itself
    PID = function_server:new(
            #mainloop_state{
                    actor           = ?NO_ACTOR,
                    next_actor      = ?NO_ACTOR,
                    is_running      = false,
                    controls        = display:get_controls( Display ),
                    frame_time      = (?ONE_SECOND div FrameRate),
                    act_timestamp   = now()
            },
            [
                { ?MAINLOOP_MESSAGE_ACT,
                    fun( _NoParams, State ) ->
                        if
                            State#mainloop_state.is_running ->
                                frame_limit( State#mainloop_state.act_timestamp, State#mainloop_state.frame_time ),
                                Time = now(),
                                case State#mainloop_state.next_actor of
                                    ?NO_ACTOR ->
                                        Actor = State#mainloop_state.actor,
                                        NewState = State#mainloop_state{
                                                act_timestamp = Time,
                                                controls = display:get_controls( Display )
                                        };
                                    NewActor ->
                                        Actor = NewActor,
                                        NewState = State#mainloop_state{
                                                actor = NewActor,
                                                next_actor = ?NO_ACTOR,
                                                act_timestamp = Time,
                                                controls = display:get_controls( Display )
                                        }
                                end,
                                
                                Sync  ! { ?USE_ACTOR, Actor },
                                Paint ! { ?USE_ACTOR, Actor },
                                Act   ! { ?USE_ACTOR, Actor },
                                
                                NewState;
                            true ->
                                State
                        end
                    end
                },
                { ?CONTROLS_HANDLER_GET_CONTROLS,
                    fun( Sender, State ) ->
                        Sender ! { ?CONTROLS_HANDLER_GET_CONTROLS_RESPONSE, State#mainloop_state.controls },
                        State
                    end
                },
                { ?MAINLOOP_MESSAGE_SET_ACTOR,
                    fun( Actor, State ) ->
                        State#mainloop_state{ next_actor = Actor }
                    end
                },
                { ?MAINLOOP_MESSAGE_STOP,
                    fun( _NoParams, State ) ->
                        State#mainloop_state{ is_running = false }
                    end
                }
            ],
            [
                { ?MAINLOOP_MESSAGE_GET_ACTOR,
                    fun( _None, State ) ->
                        { State, State#mainloop_state.actor }
                    end
                },
                { ?MAINLOOP_MESSAGE_RUN,
                    fun( Actor, State ) ->
                        if
                            State#mainloop_state.is_running ->
                                set_actor( self(), Actor ),
                                { State, ok };
                            true ->
                                NewState = State#mainloop_state{
                                    actor = Actor,
                                    is_running = true,
                                    next_actor = ?NO_ACTOR
                                },
                                act( self() ),
                                { NewState, ok }
                        end
                    end
                }
            ]
    ),
    Sync  ! { ?USE_PID, PID },
    Paint ! { ?USE_PID, PID },
    Act   ! { ?USE_PID, PID },
    PID ! ?DISPLAY_RECEIVE_GRAPHICS_FINISHED,
    PID.

%%          loop_sync
%%
%% @spec loop_sync( PID::mainloop() ) -> ok
loop_sync( PID ) ->
    receive
        {?USE_ACTOR, Actor} -> ok
    end,
    receive
        ?SYNC_ACT_DONE ->
            receive
                ?SYNC_PAINT_DONE ->
                    data_server:send( Actor, ?ACTOR_ON_END_OF_MAINLOOP ),
                    act( PID )
            end
    end,
    loop_sync( PID )
.

loop_act( Sync, Self ) ->
    receive
        {?USE_ACTOR, Actor} -> ok
    end,
    actor:act( Actor, Self ),
    Sync ! ?SYNC_ACT_DONE,
    loop_act( Sync, Self )
.

loop_paint( Display, Sync, Self ) ->
    receive
        {?USE_ACTOR, Actor} -> ok
    end,
    display:paint_actor( Display, Actor ),
    Sync ! ?SYNC_PAINT_DONE,
    loop_paint( Display, Sync, Self )
.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Private Functions
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%              frame_limit
%%
%% @doc This performs the frame limiting code to stop the mainloop running too quickly.
%% The time given should be the start time of the current frame, whilst this
%% function should be called at the end of the frame.
%%
%% Based on the difference between the start time and the current time, this
%% will try to limit the frame time down to the given FrameTime.
%% 
%% @spec frame_limit( StartTime::integer(), FrameTime::integer() ) -> ok
frame_limit( StartTime, FrameTime ) ->
    TimeDiff = erlang:round( timer:now_diff(now(), StartTime) ),
    SleepTime = FrameTime - TimeDiff,
    if
        (SleepTime > 1000) ->
            timer:sleep( SleepTime div 1000 );
        true ->
            ok
    end.

%%              act
%%
%% @doc Tells the mainloop to update it's root actor, if it has one.
%% @spec act( MainLoop::mainloop() ) -> ok
act( MainLoop ) -> data_server:send( MainLoop, ?MAINLOOP_MESSAGE_ACT ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public Functions
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%              run
%% 
%% @doc Tells the mainloop to run with the given Actor.
%% This can also be used for replacing the Actor the MainLoop is using.
%%
%% @spec run( MainLoop::mainloop(), Actor::actor() ) -> ok
run( MainLoop, Actor ) -> data_server:request( MainLoop, ?MAINLOOP_MESSAGE_RUN, Actor ).

%%              stop
%%
%% @doc Stops the mainloop running. It will subsequently no longer hold an actor.
%% @spec stop( MainLoop::mainloop() ) -> ok
stop(MainLoop) -> data_server:send( MainLoop, ?MAINLOOP_MESSAGE_STOP ).

%%              set_actor
%%
%% @doc Sets the actor to use as the root actor in this mainloop on the next cycle of acts and paints
%% @spec set_actor( MainLoop::mainloop(), Actor::actor() ) -> ok
set_actor( MainLoop, Actor ) -> data_server:send( MainLoop, ?MAINLOOP_MESSAGE_SET_ACTOR, Actor ).

%%              get_actor
%%
%% @doc Gets and returns the actor running within this mainloop.
%% @spec get_actor( MainLoop::mainloop() ) -> Actor::actor()
get_actor( MainLoop ) -> data_server:request( MainLoop, ?MAINLOOP_MESSAGE_SET_ACTOR ).

%%              get_controls
%%
%% @doc Returns a snapshot to the current state of the controls.
%% @spec get_controls( MainLoop::mainloop() ) -> Controls::controls()
get_controls( MainLoop ) -> data_server:request( MainLoop, ?CONTROLS_HANDLER_GET_CONTROLS ).
