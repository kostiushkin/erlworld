
%%%         bullet
%%%
%%% @doc This is a player bullet.
%%% It will continually move forwards from it's starting location for a limited
%%% amount of time. When this time is up, or if it has intersected with an
%%% asteroid, it will be removed from the world.
%%%
%%% If it does intersect with an asteroid it will also damage the asteroid and
%%% add a smoke explosion to the world.
%%%

-module( bullet ).

-include("blastox.hrl").

-define( SPEED, 12 ).
-define( LIFE_COUNTER_MIN, 25 ).
-define( LIFE_COUNTER_MAX, 50 ).
-define( DAMAGE, 3 ).

-define( SMOKE_LIFE_MIN, 4 ).
-define( SMOKE_LIFE_MAX, 12 ).
-define( SMOKE_COLOR, {1, 1, 1, 1} ). % white
-define( SMOKE_SIZE, 16 ).

-export([
        new/4
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          bullet
%%
%% @doc Creates a new bullet actor using the values given.
%% The Img is the image it uses for drawing. The SmokeImg is the image for it's
%% explosion it creates when it hits something. StartLocation is a tuple of
%% where it's starting in the world and the StartAngle is it's angle (in
%% radians).
%%
%% @spec new( Img::image(), SmokeImg::image(), StartLocation::{ number(), number() }, StartAngle::number() ) -> Bullet::actor()
new( Img, SmokeImg, StartLocation, StartAngle ) ->
    State = mover_state:new(
            ?NAME_BULLET, StartLocation, StartAngle, Img,
            [
                { life, rand:random(?RANDOM, ?LIFE_COUNTER_MIN, ?LIFE_COUNTER_MAX) },
                { smoke_image, SmokeImg }
            ]
    ),
    Act = fun( AS, Parent ) ->
            mover_state:act_move( act( AS, Parent ), ?SPEED )
    end,
    actor:new( Act, mover_state:new_paint(), State ).

%%          act
%%
%% @doc Updates the bullet's state. If it travels too far then it is removed.
%% If it hits an asteroid then it tells the asteroid that it has been damaged
%% and adds an explosion at the location where it hits the asteroid.
%%
%% @spec act( AS::actor_state(), Parent::world() ) -> NewAS::actor_state()
act( AS, Parent ) ->
    Life  = actor_state:get( AS, life ),
    NewAS = if
        Life < 0 ->
            world:remove_actor( Parent, actor_state:get_actor(AS) ),
            Removed = true,
            AS;
        true ->
            Removed = false,
            actor_state:set( AS, life, Life-1 )
    end,
    
    world:apply_to_intersecting_actor(
            Parent, actor_state:get_actor(AS),
            ?NAME_ASTEROID,
            fun( OtherAS ) ->
                    ( actor_state:get(OtherAS, intro_count) =< 0 )
            end,
            fun( OtherAS ) ->
                    asteroid:send_damage( actor_state:get_actor(OtherAS), ?DAMAGE ),
                    if
                        Removed ->
                            ok;
                        true ->
                            world:remove_actor( Parent, actor_state:get_actor(AS) ),
                            world:add_actor( Parent, explosion:new(
                                    actor_state:get_xy( AS ),
                                    ?SMOKE_LIFE_MIN,
                                    ?SMOKE_LIFE_MAX,
                                    actor_state:get( AS, smoke_image ),
                                    ?SMOKE_COLOR,
                                    ?SMOKE_SIZE
                            ) )
                    end
            end
    ),
    NewAS.
