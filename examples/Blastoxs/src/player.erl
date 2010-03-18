
%%%
%%%         player
%%%
%%% @doc The module for defining the player actor in the game.
%%% The player will move and shoot based on input from the controls.
%%%

-module( player ).

-include_lib("ErlWorld/include/controls.hrl").
-include("blastox.hrl").

-define( PLAYER_START_X, ?WIDTH/2  ).
-define( PLAYER_START_Y, ?HEIGHT/2 ).

-define( TURN_SPEED        , 0.075 ).
-define( START_ANGLE_OFFSET, 0.1   ).

-define( SPEED_MIN, -1.5 ).
-define( SPEED_MAX,  5   ).
-define( SPEED_INCREMENT, 0.3 ).

-define( MSG_PLAYER_KILL, kill_player_message ).

-export([
        new/1,
        send_kill/1
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          new
%%
%% @doc Returns a new actor as the player.
%% Img is the image for drawing the player. BulletImg is the image to use when
%% displaying bullets and ExplosionImg is the image to use for displaying
%% explosions.
%%
%% @spec new( {Img, BulletImg, ExplosionImg} ) -> Player::actor()
new( {Img, BulletImg, ExplosionImg} ) ->
    State = mover_state:new(
            player,
            { ?PLAYER_START_X, ?PLAYER_START_Y },
            -math:pi() / 2.0, % face up
            Img,
            [
                 { speed , {0, 0} }
            ]
    ),
    
    Act = fun(AS, Parent) ->
            C  = world:get_controls( Parent ),
            Speed = actor_state:get( AS, speed ),
            PlayerAngle = mover_state:get_angle(AS),
            
            % shooting
            case is_shooting(C) of
                true ->
                    StartLocation = get_shoot_location(
                            actor_state:get_xy(AS),
                            actor_state:get_size(AS),
                            PlayerAngle
                    ),
                    StartAngle = PlayerAngle + rand:random( ?RANDOM, -?START_ANGLE_OFFSET, ?START_ANGLE_OFFSET ),
                    world:add_actor( Parent, bullet:new(BulletImg, ExplosionImg, StartLocation, StartAngle) );
                false ->
                    ok
            end,
            
            Increment = speed_increment( is_move_forward(C), is_move_back(C) ),
            Acceleraton = mover_state:speed_to_xy( Increment, PlayerAngle ),
            NewSpeed = add_speeds( Speed, Acceleraton ),
            
            FinalAS = mover_state:move(
                    actor_state:set(
                            act_turn(
                                    AS,
                                    is_move_left(C),
                                    is_move_right(C)
                            ),
                            speed, NewSpeed ),
                    NewSpeed
            ),
            
            receive
                ?MSG_PLAYER_KILL ->
                    game:send_player_death( Parent ),
                    world:remove_actor( Parent, actor_state:get_actor(FinalAS) ),
                    world:add_actor( Parent, player_death:new(actor_state:get_xy(FinalAS), ExplosionImg) ),
                    FinalAS
            after
                1 -> FinalAS
            end
    end,
    
    actor:new( Act, mover_state:new_paint(), State ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Update Code
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          add_speeds
%%
%% @doc Adds the two tuples of locations together, but limited to the maximum speed.
%% @spec add_speeds( {SX::number(), SY::number()}, {AX::number(), AY::number()} ) -> {X::number(), Y::number()}
add_speeds( {SX, SY}, {AX, AY} ) ->
    { limit_val(SX+AX, ?SPEED_MAX), limit_val(SY+AY, ?SPEED_MAX) }.

%%          limit_val
%%
%% @doc This returns the given value limited to within the maximum given.
%% If the value is greater then Max then Max is returned. If it is less then
%% negative Max, then negative Max is returned. Otherwise the given Val is
%% returned.
%%
%% @spec limit_val( Val::number(), Max::number() ) -> LimitVal::number()
limit_val(Val, Max) when (Val < -Max) -> -Max;
limit_val(Val, Max) when (Val >  Max) ->  Max;
limit_val(Val, _Max)                  ->  Val.

%%          speed_increment
%%
%% @doc Returns the amount of speed up or down depending on the booleans given.
%% @spec speed_increment( IsForward::boolean(), IsBackward::boolean() ) -> Acceleration::number()
speed_increment( true, false ) -> ?SPEED_INCREMENT;
speed_increment( false, true ) -> -?SPEED_INCREMENT/2;
speed_increment( _IsForward, _IsBackward ) -> 0.

%%          act_turn
%%
%% @doc
%% @spec act_turn( AS::mover_state(), Left::boolean(), Right::boolean() ) -> NewAS::mover_state()
act_turn( AS, Left, Right ) -> mover_state:act_turn( AS, ?TURN_SPEED, Left, Right ).

%%          send_kill
%%
%% @doc Sends a kill message to the actor given.
%% @spec send_kill( Player::actor() ) -> ok
send_kill( Player ) -> Player ! ?MSG_PLAYER_KILL.

%%          get_shoot_location
%%
%% @doc Returns the location where the bullets will appear from underneath the player.
%% The bullets don't start at the centre of the player's image. Instead it's
%% slighty off at the nose of the player.
%%
%% @spec get_shoot_location( {X::number(), Y::number()}, {W::number(), H::number()}, Angle::number() ) -> {NewX::number(), NewY::number()}
get_shoot_location( {X, Y}, {W, _H}, Angle ) ->
    { X + (W/2)*math:cos(Angle),
      Y + (W/2)*math:sin(Angle) }.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Controls Detection Code
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          is_shooting
%%
%% @doc Returns true or false stating if the user is shooting.
%% @spec is_shooting( C::controls() ) -> boolean()
is_shooting(C)     -> controls:is_key_down(C, ?KEY_SPACE      ).

%%          is_move_forward
%%
%% @doc Returns true or false stating if the user is moving forwards.
%% @spec is_move_forward( C::controls() ) -> boolean()
is_move_forward(C) -> controls:is_key_down(C, ?KEY_UP_ARROW   ) or controls:is_key_down(C, $w).

%%          is_move_back
%%
%% @doc Returns true or false stating if the user is moving backwards.
%% @spec is_( C::controls() ) -> boolean()
is_move_back(C)    -> controls:is_key_down(C, ?KEY_DOWN_ARROW ) or controls:is_key_down(C, $s).

%%          is_move_left
%%
%% @doc Returns true or false stating if the user is turning to the left.
%% @spec is_move_left( C::controls() ) -> boolean()
is_move_left(C)    -> controls:is_key_down(C, ?KEY_LEFT_ARROW ) or controls:is_key_down(C, $a).

%%          is_move_right
%%
%% @doc Returns true or false stating if the user is turning to the right.
%% @spec is_move_right( C::controls() ) -> boolean()
is_move_right(C)   -> controls:is_key_down(C, ?KEY_RIGHT_ARROW) or controls:is_key_down(C, $d).
