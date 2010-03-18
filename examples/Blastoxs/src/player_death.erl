
%%% 
%%%         player_death
%%%
%%% @doc This is an Actor that runs the players death animation. It will
%%% randomly add explosion actors to the World it is in for a random amount of
%%% time.
%%%

-module( player_death ).
-author("Joseph Lenton").

-include("blastox.hrl").

-define( LIFE, 75 ).
-define( SPAWN_MIN, 5 ).
-define( SPAWN_MAX, 10 ).

-define( EXPLOSION_LIFE_MIN, 5 ).
-define( EXPLOSION_LIFE_MAX, 30 ).
-define( EXPLOSION_OFFSET, 18 ).
-define( EXPLOSION_COLOR, {255/255, 153/255, 38/255, 1} ). % a light orange
-define( EXPLOSION_SIZE, 64 ).

-export([
        new/2
]).

%%      new
%%
%% @doc Creates a new player death animation at the location given.
%% The image is used as the image for the explosion actors.
%%
%% @spec new( StartLocation::{ X::number(), Y::number() }, Img::image() ) -> PlayerDeath::actor()
new(StartLocation, Img) ->
    State = actor_state:new(
            ?NAME_PLAYER_DEATH, StartLocation, {1, 1},
            [
                    { image, Img },
                    { life , ?LIFE },
                    { spawn_life , 0 }
            ]
    ),
    
    Act = fun( AS, Parent ) ->
        act( AS, Parent, actor_state:get(AS, life) )
    end,
    
    Paint = fun( _AS, _G ) ->
        ok
    end,
    
    actor:new( Act, Paint, State ).

%%      act
%%
%% @doc If this player deaths time is up then it will be removed from it's world.
%% Otherwise it will randomly add explosion actors to the world it is contained
%% within.
%%
%% @spec act( AS::actor_state(), Parent::world(), Life::number() ) -> NewAS::actor_state()
act( AS, Parent, Life ) when (Life < 0) ->
    world:remove_actor( Parent, actor_state:get_actor(AS) ),
    AS;
act( AS, Parent, Life ) ->
    SpawnLife = actor_state:get( AS, spawn_life )-1,
    actor_state:set(
            if
                (SpawnLife =< 0) ->
                    world:add_actor( Parent, new_explosion(AS) ),
                    actor_state:set(
                            AS,
                            [
                                    { spawn_life, rand:random(?RANDOM, ?SPAWN_MIN, ?SPAWN_MAX) }
                            ]
                    );
                true ->
                    actor_state:set( AS, spawn_life, SpawnLife )
            end,
            life,
            Life-1
    ).

%%      new_explosion
%%
%% @doc Creates and returns a new explosion actor at a random offset to the actor state given.
%% @spec new_explosion( AS::actor_state() ) -> Explosion::explosion()
new_explosion(AS) ->
    Img = actor_state:get(AS, image),
    {X, Y} = actor_state:get_xy( AS ),
    explosion:new(
        {
                X + rand:random( ?RANDOM, -?EXPLOSION_OFFSET, ?EXPLOSION_OFFSET ),
                Y + rand:random( ?RANDOM, -?EXPLOSION_OFFSET, ?EXPLOSION_OFFSET )
        },
        ?EXPLOSION_LIFE_MIN, ?EXPLOSION_LIFE_MAX,
        Img,
        ?EXPLOSION_COLOR,
        ?EXPLOSION_SIZE
    ).
