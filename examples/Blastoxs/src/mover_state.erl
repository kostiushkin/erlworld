
%%%
%%%         mover_state
%%%
%%% @doc A module that builds on top of the actor_state module.
%%% It adds some common movement code for moving the actor state around.
%%%

-module( mover_state ).

-author("Joseph Lenton").

-include("mover_state.hrl").
-include("blastox.hrl").

-export([
      new/4
    , new/5
    , new_paint/0

    , set_xy/2, set_xy/3
    , move/2, move/3
    
    , act_move/2
    , act_turn/4

    , get_angle/1
    , get_img/1

    , limit_xy/1
    , speed_to_xy/2
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          new
%%
%% @doc The same as the other new function, only with no properties.
%% @spec new( Name::atom(), StartXY::{ number(), number() }, StartAngle::number(), Img::image() ) -> mover_state()
new( Name, StartXY, StartAngle, Img ) ->
    new( Name, StartXY, StartAngle, Img, [] ).

%%          new
%%
%% @doc Creates a new mover state at the location given.
%% It will store the image and the angle given using these for drawing. The
%% values in the properties list will also be added to the state.
%%
%% The StartAngle is in radians.
%%
%% @spec new( Name::atom(), StartXY::{ number(), number() }, StartAngle::number(), Img::image(), Properties::[ {Property::atom(), Value::term()} ] ) -> mover_state()
new( Name, StartXY, StartAngle, Img, Properties ) ->
    actor_state:set(
        actor_state:new(
                Name,
                StartXY,
                image:get_size(Img),
                [
                    { ?ACTOR_IMG, Img },
                    { ?ACTOR_ANGLE, StartAngle }
                ]
        ),
        Properties
    ).

%%          new_paint
%%
%% @doc Creates a fun that can be used for painting a mover_state.
%% @spec new_paint() -> ( AS::actor_state(), G::graphics() ) -> ok
new_paint() ->
    fun( AS, G ) ->
        {X, Y} = actor_state:get_xy(AS),
        Max = max( actor_state:get_size(AS) ),
        Angle = get_angle(AS),
        Img = get_img(AS),
        
        if
            (X < Max/2         ) -> graphics:draw_image_rotated( G, Img, {X+?WIDTH, Y}, Angle );
            (X > ?WIDTH-(Max/2)) -> graphics:draw_image_rotated( G, Img, {X-?WIDTH, Y}, Angle );
             true                -> ok
        end,
        if
            (Y < Max/2          ) -> graphics:draw_image_rotated( G, Img, {X, Y+?HEIGHT}, Angle );
            (Y > ?HEIGHT-(Max/2)) -> graphics:draw_image_rotated( G, Img, {X, Y-?HEIGHT}, Angle );
             true                 -> ok
        end,
        graphics:draw_image_rotated( G, Img, {X, Y}, Angle )
    end.

%%          max
%%
%% @doc The same as the built in max function, but works on two numbers supplied as a tuple.
%% @spec max( {A::number(), B::number()} ) -> number()
max( {A, B} ) when A > B -> B;
max( {A, _B})            -> A.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Acting
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          act_move
%%
%% @doc Moves the given mover_state by the speed given.
%% The angle it will travel in is the angle stored within it.
%%
%% @spec act_move( AS::mover_state(), Speed::number() ) -> NewAS::mover_state()
act_move( AS, Speed ) ->
    set_xy(
            AS,
            calculate_move( actor_state:get_xy(AS), Speed, get_angle(AS) )
    ).

%%          move
%%
%% @doc Moves the actor state by the amount given, but the location is wrapped to within the display size.
%% @spec move( AS::actor_state(), X::number(), Y::number() ) -> NewAS::actor_state()
move( AS, X, Y ) ->
    move( AS, {X, Y} ).

%%          move
%%
%% @doc Moves the actor state by the amount given, but the location is wrapped to within the display size.
%% @spec move( AS::actor_state(), {X::number(), Y::number()} ) -> NewAS::actor_state()
move( AS, {X, Y} ) ->
    { X2, Y2 } = actor_state:get_xy(AS),
    set_xy( AS, {X+X2, Y+Y2} ).

%%          set_xy
%%
%% @doc The same as the other set_xy, only this takes the X and Y values seperately.
%% @spec set_xy( AS::actor_state(), X::number(), Y::number() ) -> NewAS::actor_state()
set_xy( AS, X, Y ) ->
    set_xy( AS, {X, Y} ).

%%          set_xy
%%
%% @doc Moves the mover_state to the location given, but wrapped within the displays size.
%% @spec set_xy( AS::actor_state(), XY::{ number(), number() } ) -> NewAS::actor_state()
set_xy( AS, XY ) ->
    actor_state:set_xy(
            AS,
            limit_xy( XY )
    ).

%%          calculate_move
%%
%% @doc Adds the speed and angle to the X and Y values given.
%% Essentially returning the result of moving from the X and Y values in the
%% angle given by the given speed. The angle is in radians.
%%
%% @spec calculate_move( {X::number(), Y::number()}, Speed::number(), Angle::number() ) -> { NewX::number(), NewY::number() }
calculate_move( {X, Y}, Speed, Angle ) -> {X + Speed*math:cos(Angle), Y+Speed*math:sin(Angle) }.

%%          speed_to_xy
%%
%% @doc Returns a tuple of the X and Y movement of moving in the given angle by the given speed.
%% The angle is in radians.
%%
%% @spec speed_to_xy( Speed::number(), Angle::number() ) -> { X::number(), Y::number() }
speed_to_xy( Speed, Angle ) -> { Speed*math:cos(Angle), Speed*math:sin(Angle) }.

%%          act_turn
%%
%% @doc Turns the given state left, right or not at all based on the values of left and right.
%% The result of turning (or not turning) is returned as a new mover_state.
%%
%% The amount to turn is a radian whilst the left and right values should be
%% booleans.
%%
%% @spec act_turn( AS::mover_state(), Turn::number(), Left::boolean(), Right::boolean() ) -> NewAS::mover_state()
act_turn( AS, Turn, Left, Right ) ->
    actor_state:set( AS, ?ACTOR_ANGLE,
            act_turn_inner(
                    get_angle( AS ),
                    Turn,
                    Left,
                    Right
            )
    ).

%%          act_turn_inner
%%
%% @doc Returns Angle plus or minus turn based on the values of Left and Right.
%% @spec act_turn_inner( Angle::number(), Turn::number(), Left::boolean(), Right::boolean() ) -> NewAngle::number()
act_turn_inner( Angle, _Turn, true, true    ) -> Angle;
act_turn_inner( Angle, Turn, true, _Right   ) -> Angle - Turn;
act_turn_inner( Angle, Turn, _Left, true    ) -> Angle + Turn;
act_turn_inner( Angle, _Turn, _Left, _Right ) -> Angle.

%%          limit_xy
%%
%% @doc Returns a tuple where the given X and Y values wrap around the width and height of the display.
%% @spec limit_xy( {X::number(), Y::number()} ) -> {NewX::number(), NewY::number()}
limit_xy( {X, Y} ) -> { limit(X, ?WIDTH), limit(Y, ?HEIGHT) }.

%%          limit
%%
%% @doc Returns the given value but translated to between 0 and Max.
%% If val is bigger then max then val-max is returned, not max. Similar if val
%% is less then 0; val+max is returned not 0.
%%
%% If val is between 0 and max then val is returned unchanged.
%%
%% @spec limit( Val::number(), Max::number() ) -> NewVal::number()
limit( Val, Max  ) when Val > Max -> Val-Max;
limit( Val, Max  ) when Val < 0   -> Val+Max;
limit( Val, _Max )                -> Val.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Getters
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          get_img
%%
%% @doc Return the image to use when drawing this mover.
%% @spec get_img( AS::mover_state() ) -> Image::image()
get_img(AS) -> actor_state:get( AS, ?ACTOR_IMG ).

%%          get_angle
%%
%% @doc Returns the angle set in this mover, in radians.
%% @spec get_angle( AS::mover_state() ) -> Angle::number()
get_angle(AS) -> actor_state:get( AS, ?ACTOR_ANGLE ).