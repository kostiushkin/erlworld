
%%%
%%%         asteroid
%%%
%%% This is an actor for the enemies in the game. It moves around the world
%%% slowly spinning looking to see if it has collided into the player. It also
%%% receives death values from bullets which lower it's hp. When it's hp reaches
%%% zero it will be removed from the world and inform the game.
%%%

-module( asteroid ).

-include("blastox.hrl").

-define( TURN_SPEED, 0.1 ).
-define( SPEED_MIN, 1 ).
-define( SPEED_MAX, 4 ).

-define( INTRO_COUNT, 160 ).

-define( HEALTH_MIN, 20 ).
-define( HEALTH_MAX, 30 ).

-define( MSG_DAMAGE, damage_asteroid_message ).

-export([
        new/1,
        send_damage/2
]).

%%          new
%%
%% @doc Creates a new asteroid, the image is the image this will use for when it's drawn.
%% @spec new( Img::image() ) -> actor()
new( Img ) ->
    %State
    State = mover_state:new(
        ?NAME_ASTEROID,
        new_start_location(),
        new_start_angle(),
        Img,
        [
            { intro_count, ?INTRO_COUNT },
            { draw_angle, new_start_angle() },
            { draw_turn_speed, rand:random(?RANDOM, -?TURN_SPEED, ?TURN_SPEED) },
            { speed, rand:random(?RANDOM, ?SPEED_MIN, ?SPEED_MAX) },
            { health, rand:random(?RANDOM, ?HEALTH_MIN, ?HEALTH_MAX) }
        ]
    ),
    
    % Update
    ActFun = fun( AS, Parent ) ->
        act_receive_damage( act_intro_count(act_move_turn(AS)), Parent )
    end,

    %Paint
    PaintFun = fun( AS, G ) ->
        IntroCount = actor_state:get( AS, intro_count ),
        Alpha = if
            IntroCount > 0 ->
                float(?INTRO_COUNT-IntroCount) / float(?INTRO_COUNT);
            true ->
                1.0
        end,

        graphics:set_color( G, 1, 1, 1, Alpha ),
        graphics:draw_image_rotated( G, mover_state:get_img(AS), actor_state:get_xy(AS), actor_state:get(AS, draw_angle) ),
        graphics:set_color( G, 1, 1, 1, 1 )
    end,

    actor:new( ActFun, PaintFun, State ).

%%          new_start_location
%%
%% @doc Returns a random location within the display.
%% @spec new_start_location() -> { X::number(), Y::number() }
new_start_location() -> { rand:random(?RANDOM, ?WIDTH), rand:random(?RANDOM, ?HEIGHT) }.

%%          new_start_angle
%%
%% @doc Returns a random angle from 0 to 2 Pi. This is in radians.
%% @spec new_start_angle() ->
new_start_angle() -> rand:random(?RANDOM, math:pi()*2).

%%          act_intro_count
%%
%% @doc Updates the intro counter for the asteroid, updating it fading into the world.
%% @spec act_intro_count( AS::actor_state() ) -> NewAS::actor_state()
act_intro_count( AS ) ->
    IntroCount = actor_state:get(AS, intro_count),
    if
        (IntroCount < 0) ->
            AS;
        true ->
            actor_state:set( AS, intro_count, IntroCount-1 )
    end.

%%          act_receive_damage
%%
%% @doc Checks if this has received damage, and if so updates it's state.
%% @spec act_receive_damage( AS::actor_state(), Parent::world() ) -> NewAS::actor_state()
act_receive_damage( AS, Parent ) ->
    HP = actor_state:get( AS, health ),
    if
        HP > 0 ->
            receive
                { ?MSG_DAMAGE, Damage } ->
                    if
                        (HP-Damage) < 0 ->
                            game:send_asteroid_death( Parent ),
                            world:remove_actor( Parent, actor_state:get_actor(AS) ),
                            AS;
                        true ->
                            act_check_collision( actor_state:set(AS, health, HP-Damage), Parent )
                    end
                after 1 ->
                    act_check_collision( AS, Parent )
            end;
        true ->
            act_check_collision( AS, Parent )
    end.

%%          act_check_collision
%%
%% @doc Checks if a collision has occurred with a player actor.
%% If so then the player is told to die.
%%
%% @spec act_check_collision( AS::actor_state(), Parent::world() ) -> NewAS::actor_state()
act_check_collision( AS, Parent ) ->
    act_check_collision( AS, Parent, actor_state:get(AS, intro_count) ).

%%          act_check_collision
%%
%% @doc If this asteroid is in it's intro state then nothing will happen.
%% Otherwise it will check for a collision with the player and then tell it to
%% die.
%%
%% @spec act_check_collision( AS::actor_state(), Parent::world(), IntroCount::number() ) -> NewAS::actor_state()
act_check_collision(AS, Parent, IntroCount) when (IntroCount =< 0) ->
    world:apply_to_intersecting_actor(
            Parent, actor_state:get_actor(AS),
            ?NAME_PLAYER,
            fun( OtherAS ) ->
                    player:send_kill( actor_state:get_actor(OtherAS) ),
                    world:remove_actor( Parent, actor_state:get_actor(AS) )
            end
    ),
    AS;
act_check_collision(AS, _Parent, _IntroCount) -> AS.

%%          act_move_turn
%%
%% @doc Updates the movement and the turning of the actor.
%% @spec act_move_turn( AS::actor_state() ) -> NewAS::actor_state()
act_move_turn( AS ) ->
    mover_state:act_move(
            actor_state:set( AS, draw_angle,
                    actor_state:get( AS, draw_angle ) + actor_state:get( AS, draw_turn_speed )
            ),
            actor_state:get( AS, speed )
    ).

%%          send_damage
%%
%% @doc Sends a damage message to the asteroid given.
%% @spec send_damage( Asteroid::actor(), Damage::number() ) -> ok
send_damage( Asteroid, Damage ) -> Asteroid ! { ?MSG_DAMAGE, Damage }.
