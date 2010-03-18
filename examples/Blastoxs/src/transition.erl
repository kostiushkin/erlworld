
%%%
%%%         transition
%%%
%%% @doc This actor is for the visual transition from one state to another.
%%% It does this drawn as tiled black squares across the whole screen.
%%%
%%% When the transition is over it will message the game to say it has finished.
%%% 

-module( transition ).

-include("blastox.hrl").

-define( MSG_TIMEOUT, timeout_message ).

% The size of the tiled transition rectangles across the screen.
% 40 is used because it divides with no remainder into the current ?WIDTH and ?HEIGHT.
-define( RECT_SIZE, 40 ).

-export([
        new/1
]).

%%          new
%%
%% @doc Creates a new transition that will last for the number of ticks given.
%% One tick is one frame, so 60 ticks will run for 60 frames.
%% 
%% @spec new( Ticks::integer() ) -> Transition::actor()
new( Ticks ) ->
    State = actor_state:new(
            transition, {0, 0}, {1, 1},
            [
                    { is_intro, true },
                    { ticks, Ticks },
                    { max_ticks, Ticks }
            ] ),
    
    Act   = fun(AS, Parent) -> act(AS, Parent) end,
    Paint = fun(AS, G) -> paint(AS, G) end,
    
    actor:new( Act, Paint, State ).

%%          act
%%
%% @doc Updates the ticks that this transition has survived for.
%% @spec act( AS::actor_state(), Parent::world() ) -> NewAS::actor_state()
act(AS, Parent) -> act_ticks( AS, Parent, get_ticks(AS) ).

%%          act_ticks
%%
%% @doc The actual tick updating code for the act function.
%% But this is based on the actual number of ticks given.
%%
%% @spec act_ticks( AS::actor_state(), Parent::world(), Ticks::number() ) -> NewAS::actor_state()
act_ticks( AS,  Parent,  Ticks ) when (Ticks =< 0) -> act_timeout( AS, Parent, is_intro(AS) );
act_ticks( AS, _Parent, _Ticks )                   -> actor_state:set( AS, ticks, get_ticks(AS)-1 ).

%%          act_timeout
%%
%% @doc The code that is performed when this actor has run out of ticks.
%% If it's in the intro state then the tick count is reset to it's maximum and
%% the actor_state enters it's endtro state. Otherwise it calls to remove itself
%% from it's world.
%%
%% @spec act_timeout( AS::actor_state(), Parent::world(), IsIntro::boolean() ) -> NewAS::actor_state()
act_timeout( AS, Parent, true ) ->
    game:send_end_transition( Parent ),
    actor_state:set( AS, [
        { ticks, get_max_ticks(AS) },
        { is_intro, false }
    ] );
act_timeout( AS, Parent, _IsIntro ) ->
    world:remove_actor( Parent, actor_state:get_actor(AS) ),
    AS.

%%          paint
%%
%% @doc Paints the squared transition across the screen, with a grey outline on each square.
%% @spec paint( AS::actor_state(), G::graphics() ) -> ok
paint(AS, G) ->
    T = get_transition(AS),
    W = T*?RECT_SIZE,
    H = T*?RECT_SIZE,
    
    for2( ?WIDTH / ?RECT_SIZE, ?HEIGHT / ?RECT_SIZE,
            fun( X, Y ) ->
                DrawX = X*?RECT_SIZE,
                DrawY = Y*?RECT_SIZE,
                
                % inner
                graphics:set_color( G, color:black() ),
                graphics:fill_rect( G, DrawX - (W-?RECT_SIZE)/2, DrawY - (H-?RECT_SIZE)/2, W, H ),
                % outline
                graphics:set_color( G, color:grey() ),
                graphics:draw_rect( G, DrawX - (W-?RECT_SIZE)/2, DrawY - (H-?RECT_SIZE)/2, W, H )
            end
    ),
    graphics:set_color( G, color:white() ).

%%          for2
%%
%% @doc A 2-dimensional for-loop. It's essentially two nested for loops.
%% @spec for2( MaxI::number(), MaxJ::number(), Fun::( I::number(), J::number() ) -> term() ) -> term()
for2( MaxI, MaxJ, Fun ) ->
    util:for( MaxI, fun(I) ->
            util:for( MaxJ, fun(J) ->
                    Fun( I, J )
            end )
    end ).

%%          is_intro
%%
%% @doc Returns true if this is in it's intro state, otherwise false.
%% @spec is_intro( AS::actor_state() ) -> boolean()
is_intro(AS) -> actor_state:get( AS, is_intro ).

%%          get_ticks
%%
%% @doc Returns the number of frame ticks this intro has left to run for.
%% @spec get_ticks( AS::actor_state() ) -> Ticks::integer()
get_ticks(AS) -> actor_state:get( AS, ticks ).

%%          get_max_ticks
%%
%% @doc Returns the maximum number of ticks that this actor was initialized with.
%% @spec get_max_ticks( AS::actor_state() ) -> integer()
get_max_ticks(AS) -> actor_state:get( AS, max_ticks ).

%%          get_transition
%%
%% @doc Returns the percent of transition to use for multiplying when painting.
%% @spec get_transition( AS::actor_state() ) -> number()
get_transition(AS) -> calculate_transition( get_ticks(AS), get_max_ticks(AS), is_intro(AS) ).

%%          calculate_transition
%%
%% @doc This does the actual calculation for the get_transition function.
%% @spec calcualte_transition( Ticks::number(), MaxTicks::number(), IsIntro::boolean() ) -> number()
calculate_transition( Ticks, MaxTicks, true ) -> 1.0 - (Ticks / MaxTicks);
calculate_transition( Ticks, MaxTicks, _Is  ) ->       (Ticks / MaxTicks).
