
%%%
%%%         star
%%%
%%% @doc This module is for creating and updating the backgound stars in the
%%% game. They continually move from right to left along the screen and wrap
%%% around when they hit the left edge of it.
%%%
%%% They are drawn as a line which is dependent on it's speed. The faster the
%%% star is travelling, the longer it's line. It is also more white when fast
%%% and more blue when slow.
%%%

-module( star ).

-include("blastox.hrl").

-define( MIN_SPEED, 4 ).
-define( MAX_SPEED, 12 ).

-define( LENGTH, 6 ).

-export([
        new/0
]).

%%          new
%%
%% @doc Returns a new star with a random location and speed.
%% @spec new() -> Star::actor()
new() ->
    XY = { rand:random(?RANDOM, ?WIDTH), rand:random(?RANDOM, ?HEIGHT) },
    Size = {1, 1},
    State = actor_state:new( ?NAME_STAR, XY, Size,
            [ {speed, new_speed()} ]
    ),
    
    Act = fun(AS, _Parent) ->
        Speed = actor_state:get(AS, speed),
        X = actor_state:get_x(AS) - Speed,
        if
            (X < 0) ->
                actor_state:set(
                        actor_state:set_x( AS, X+?WIDTH ),
                        speed,
                        new_speed()
                );
             true -> actor_state:set_x( AS, X )
        end
    end,

    Paint = fun(AS, G) ->
        {X, Y} = actor_state:get_xy( AS ),
        SpeedPercent = actor_state:get(AS, speed) / ?MAX_SPEED,
        Length = ?LENGTH*SpeedPercent,
        graphics:set_color( G, SpeedPercent, SpeedPercent, 1, 1 ),
        graphics:draw_line( G, X, Y, X+Length, Y ),
        graphics:set_color( G, 1, 1, 1 )
    end,

    actor:new( Act, Paint, State ).

%%          new_speed
%%
%% @doc Returns a random speed between the defined minimum and maximum speed values.
%% @spec new_speed() -> number()
new_speed() -> rand:random( ?RANDOM, ?MIN_SPEED, ?MAX_SPEED ).