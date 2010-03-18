
%%%         explosion
%%%
%%% @doc For displaying explosions in the world.
%%% An explosion is displayed as a round image that scales into life from a
%%% single point and then fades out into nothing.
%%% 

-module( explosion ).

-include("blastox.hrl").

-export([
        new/6, new/7
]).

%%          new
%%
%% @doc Creates a new explosion with the properties given.
%% It will appear at the location given and last for a random amount of time
%% between MinLife and MaxLife. It will draw using the given image coloured by
%% the given amount. The size states the maximum size this explosion will expand
%% too.
%%
%% @spec new( X::number(), Y::number(), MinLife::number(), MaxLife::number(), Img::image(), Color::color(), Size::number() ) -> Explosion::actor()
new( X, Y, MinLife, MaxLife, Img, Color, Size ) -> new( {X, Y}, MinLife, MaxLife, Img, Color, Size ).

%%          new
%% 
%% @doc The same as the other new function, only this has the X and Y supplied together as a tuple.
%% @spec new( XY::{ number(), number() }, MinLife::number(), MaxLife::number(), Img::image(), Color::color(), Size::number() ) -> Explosion::actor()
new( XY, MinLife, MaxLife, Img, Color, Size ) ->
    Life = rand:random( ?RANDOM, MinLife, MaxLife ),
    Bounds = {1, 1},
    State = actor_state:new( smoke, XY, Bounds,
            [ 
                { image, Img },
                { life, Life },
                { max_life, Life },
                { state, intro },
                { color, Color },
                { size, Size }
            ]
    ),
    
    Act = fun(AS, Parent) -> act( AS, Parent ) end,
    Paint = fun(AS, G) -> paint( AS, G ) end,
    
    actor:new( Act, Paint, State ).

%%          act
%%
%% @doc Updates this explosions state.
%% @spec ( AS::actor_state(), Parent::world() ) -> NewAS::actor_state()
act(AS, Parent) -> act( AS, Parent, actor_state:get(AS, life), actor_state:get(AS, state) ).

%%          act
%%
%% @doc Updates according to one of three different states.
%% If the explosion is in it's intro state and it's life has expired, then it
%% will move into it's endtro state. If it's at the end of it's endtro state,
%% then it will be removed.
%%
%% Otherwise it's life count is updated.
%%
%% @spec act( AS::actor_state(), Parent::world(), Life::integer(), intro | endtro ) -> NewAS::actor_state()
act(AS, _Parent, Life, intro) when (Life =< 0) ->
    actor_state:set( AS,
                    [
                        { state, endtro },
                        { life, actor_state:get(AS, max_life) }
                    ]
    );
act(AS, Parent, Life, endtro) when (Life =< 0) ->
    world:remove_actor( Parent, actor_state:get_actor(AS) ),
    AS;
act(AS, _Parent, Life, _IsIntro) -> actor_state:set( AS, life, Life-1 ).

%%          paint
%%
%% @doc Paints the explosion to the screen using it's current settings.
%% @spec paint( AS::actor_state(), G::graphics() ) -> ok
paint( AS, G ) ->
    paint(
            actor_state:get_xy(AS),
            actor_state:get(AS, life),
            actor_state:get(AS, max_life),
            actor_state:get(AS, state),
            actor_state:get(AS, image),
            actor_state:get(AS, color),
            actor_state:get(AS, size),
            G
    ).

%%          paint
%%
%% @doc Paints this explosion according to it's current state.
%% If it's in it's intro state then it will be painted being scaled up from a
%% single point to it's full size. If it's in it's endtro state then it will be
%% painted fading out to be invisible.
%%
%% @spec paint( {X::number(), Y::number()}, Life::integer(), MaxLife::integer(), intro | endtro, Img::image(), Color::color(), Size::number(), G::graphics() ) -> ok
paint( {X, Y}, Life, MaxLife, intro, Img, Color, Size, G ) ->
    Mult = 1 - float(Life)/float(MaxLife),
    graphics:set_color( G, Color ),
    graphics:draw_image( G, Img, X, Y, Size*Mult, Size*Mult, true ),
    graphics:set_color( G, color:white() );
paint( {X, Y}, Life, MaxLife, endtro, Img, C, Size, G ) ->
    Alpha = float(Life) / float(MaxLife),
    graphics:set_color( G, color:get_red(C), color:get_green(C), color:get_blue(C), Alpha ),
    graphics:draw_image( G, Img, X, Y, Size, Size, true ),
    graphics:set_color( G, color:white() ).
