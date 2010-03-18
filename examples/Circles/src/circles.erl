
%%%-------------------------------------------------------------------
%%%         Circles
%%%
%%% @doc This is an ErlWorld example scenario of Circles moving around
%%% the screen.
%%%
%%% You can add circles by left-clicking anywhere in the window. The
%%% circles will be created in the direction that the mouse was last
%%% moving in. The circles are constantly pulled down by gravity.
%%%
%%% @author Joseph Lenton - JL235@Kent.ac.uk
%%%-------------------------------------------------------------------
-module( circles ).
-author("Joseph Lenton").

-include_lib("ErlWorld/include/controls.hrl").

% values for setting up the display
-define( WIDTH , 800 ).
-define( HEIGHT, 600 ).
-define( TITLE , "Circles" ).

% the name of the random number generator
-define( RANDOM, random_server ).

% values for calculating random numbers
-define( SIZE_MIN , 4  ).
-define( SIZE_MAX , 36 ).

-define( COLOR_MIN, 0.4 ).
-define( COLOR_MAX, 0.8 ).

-define( SPEED_MIN, 1.5 ).
-define( SPEED_MAX, 2.5 ).

% the minimum amount of time that must pass between creating circles, in _microseconds_
-define( MIN_TIMEOUT, 50000 ).

-define( GRAVITY, 0.0000098 ).

-export([ start/0 ]).

%%          start
%%
%% @doc Starts the Circles scenario.
%% @spec start() -> ok
start() ->
    Random = rand:new(),
    register( ?RANDOM, Random ),

    Display = display:new( ?WIDTH, ?HEIGHT, ?TITLE ),
    MainLoop = mainloop:new( Display ),
    CircleImg = image:new( Display, "circle.png" ),
    mainloop:run( MainLoop, new_world(CircleImg) ).

%%          new_world
%%
%% @doc Creates and returns a World actor ready and setup for running the scenario.
%% @spec new_world(CircleImg::image()) -> Game::world()
new_world(CircleImg) ->
%    World = world:new(),
    World = world:new(
            fun(State, _P) -> world_state:act_actors(State) end,
            fun(State, G)  ->
                graphics:set_clear_color(G, color:white()),
                world_state:paint_actors(State, G)
            end
    ),
    world:add_actor( World, new_mouse(CircleImg) ),
    World.

%%          new_mouse
%%
%% @doc Creates the mouse actor that will listen to input and create the circles.
%% @spec new_mouse(CircleImg::image()) -> Mouse::actor()
new_mouse(CircleImg) ->
    Act = fun( AS, Parent ) ->
        Controls = actor:get_controls( Parent ),
        LastLocation = actor_state:get_xy( AS ),
        Location = controls:get_mouse_xy( Controls ),
        LastTime = actor_state:get( AS, last_time ),
        Now = now(),
        Diff = timer:now_diff( Now, LastTime ),

        NewAS = if
            (Diff > ?MIN_TIMEOUT) ->
                IsButtonPress = controls:is_mouse_down(Controls, ?MOUSE_LEFT),
                add_circle( Parent, CircleImg, Location, LastLocation, IsButtonPress ),
                actor_state:set( AS, last_time, Now );
            true ->
                AS
        end,
        
        actor_state:set_xy(
                actor_state:set( NewAS, last_location, LastLocation ),
                Location )
    end,
    Paint = fun (_AS, _G) -> ok end,
    State = actor_state:new([
            { last_location, {0, 0} },
            { last_time    , now()  }
    ]),
    actor:new( Act, Paint, State ).

%%          add_circle
%%
%% @doc If the mouse is pressed, then this will add the circle to the given world.
%% @spec add_circle( World::world(), CircleImg::image(), Location::{X, Y}, LastLocation::{X, Y}, IsMouseDown::boolean() ) -> ok
add_circle( World, CircleImg,  Location,  LastLocation, true  ) ->
    Circle = new_circle( CircleImg, Location, locations_to_angle(LastLocation, Location) ),
    world:add_actor( World, Circle );
add_circle( _World, _CircleImg, _Location, _LastLocation, false ) -> ok.

%%          locations_to_angle
%%
%% @doc Converts the two locations given to an angle from the first location to the second.
%% The angle is in radians.
%%
%% @spec locations_to_angle( {X, Y}, {ToX, ToY} ) -> Angle::number()
locations_to_angle( {X, Y}, {ToX, ToY} ) -> math:atan2( ToY-Y, ToX-X ).

%%          new_circle
%% 
%% @doc Returns a new circle actor at the given location moving in the given direction.
%% @spec new_circle( CircleImg::image(), XY::{number(), number()}, Angle::number() ) -> Circle::actor()
new_circle(CircleImg, XY, Angle) ->
    Speed  = random_speed(),
    Radius = random_radius(),
    Color  = { random_color(), random_color(), random_color(), random_color() },
    Size   = { Radius*2, Radius*2 },
    Weight = Radius*Radius*math:pi(),
    Delta  = { Speed*math:cos(Angle), Speed*math:sin(Angle) },
    Gravity = weight_to_delta(Weight),
    
    State  = actor_state:new( circle, XY, Size, [
            { delta  , Delta  }
    ]),
    
    Act = fun( AS, _Parent ) ->
        {DeltaX, DeltaY} = actor_state:get( AS, delta ),
        { X, Y } = actor_state:get_xy( AS ),

        {NewDeltaX, NewX} = update_delta( DeltaX, X, Radius, ?WIDTH  ),
        {NewDeltaY, NewY} = update_delta( DeltaY+Gravity, Y, Radius, ?HEIGHT ),

        NewAS = actor_state:set_xy( AS, NewX, NewY ),
        actor_state:set( NewAS, delta, {NewDeltaX, NewDeltaY} )
    end,

    Paint = fun( AS, G ) ->
        graphics:set_color( G, Color ),
        graphics:draw_image( G, CircleImg, actor_state:get_xy(AS), actor_state:get_size(AS), true )
    end,
    
    actor:new( Act, Paint, State ).

%%          weight_to_delta
%%
%% @doc Given a weight value this will return how much force will be pulling
%% down on it in this circles scenario.
%% @spec weight_to_delta( Weight::number() ) -> Force::number()
weight_to_delta( Weight ) -> ?GRAVITY * Weight.

%%          update_delta
%%
%% @doc A helper function for updating both the movement and position of an X or
%% Y value. If the value is outside the radius or length-radius then it will be
%% returned to whichever value is closer with the delta inverted.
%%
%% Otherwise the same delta given is also returned and the value given is
%% returned with the delta value added on to it.
%%
%% @spec update_delta( Detla::number(), Val::number(), Radius::number(), Length::number() ) -> { NewDelta::number(), NewVal::number() }
update_delta( Delta, Val,  Radius, _Length ) when (Val < Radius) -> {-Delta*0.95, Radius};
update_delta( Delta, Val,  Radius,  Length ) when (Val > Length-Radius) -> {-Delta*0.95, Length-Radius};
update_delta( Delta, Val, _Radius, _Length ) -> { Delta, Val+Delta }.

%%          random_radius
%%
%% @doc Returns a random number to use for the radius of a circle.
%% @spec random_radius() -> number()
random_radius() -> rand:random( ?RANDOM, ?SIZE_MIN , ?SIZE_MAX  ).

%%          random_color
%%
%% @doc Returns a random number to use for random colour components.
%% @spec random_color() -> number()
random_color()  -> rand:random( ?RANDOM, ?COLOR_MIN, ?COLOR_MAX ).

%%          random_speed
%%
%% @doc Returns a random number to use for starting circle speeds.
%% @spec random_speed() -> number()
random_speed()  -> rand:random( ?RANDOM, ?SPEED_MIN, ?SPEED_MAX ).
