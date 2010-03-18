
%%% 
%%%         pong
%%%
%%% @doc This is an example scenario implementing the classic Pong. It has a
%%% very simplistic enemy AI to play against (which is pretty easy).
%%%
%%% To run just use the start function.
%%%
%%% @author Joseph Lenton
%%%

-module( pong ).

-export([
        start/0
]).

-define( WIDTH, 800 ).
-define( HEIGHT, 600 ).
-define( TITLE, "Pong" ).

-define( BACKGROUND_COLOR, {1, 0.917, 0.717} ). % a light wheat color
-define( BACKGROUND_CIRCLE_COLOR, {0.956, 0.643, 0.376} ). % a light orange
-define( BACKGROUND_BOTTOM_WIDTH, ?WIDTH ).
-define( BACKGROUND_BOTTOM_HEIGHT, 317 ).
-define( BACKGROUND_CIRCLE_DIAMETER, 1940 ).

-define( INCREMENT_SCORE, increment_score_msg ).

% how far in from the left or right the batons start at
-define( BATON_X, 100 ).

-define( SCORE_TEXT_COLOR, {0.2, 0.2, 0.2} ).
% the top left corner of the scoreboard
-define( SCORE_X, 20 ).
-define( SCORE_Y, 20 ).

-define( BALL_SPEED, 8 ).

-define( AI_BATON_SPEED, 12 ).

% the messages are defined so that when I use them it checks if the macro exists
-define( BALL_COLLISION_MSG, ball_collision_msg ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      start
%%
%% @doc Creates and starts the Pong game.
%% @spec start() -> term()
start() ->
    Display = display:new( ?WIDTH, ?HEIGHT, ?TITLE ),
    
    BackgroundPetal = image:new( Display, "images/background_petal.png"  ),
    BatonLeftImg    = image:new( Display, "images/baton_left.png"        ),
    BatonRightImg   = image:new( Display, "images/baton_right.png"       ),
    BallImg         = image:new( Display, "images/ball.png"              ),
    
    Background = new_background( BackgroundPetal ),
    Score = new_score( Display ),
    Actors = [
            Background,
            Score,
            new_player_baton( BatonLeftImg ),
            new_ai_baton( BatonRightImg ),
            new_ball( BallImg, Score )
    ],
    TempWorldState = world_state:set_paint_order( world_state:new(Actors), Score     ,  1 ),
    WorldState     = world_state:set_paint_order( TempWorldState         , Background, -1 ),
    World = world:new( WorldState ),
    
    Mainloop = mainloop:new( Display ),
    mainloop:run( Mainloop, World ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Background
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      new_background
%%
%% @doc Creates and returns an actor for the background.
%% @spec new_background( PetalImg::image ) -> Background::actor()
new_background( PetalImg ) ->
    Act = fun( AS, _Parent ) -> AS end, % an act function that does nothing
    Paint = fun( _AS, G ) ->
            % color background
            graphics:set_color( G, ?BACKGROUND_COLOR ),
            graphics:fill( G ),

            % draw the bottom circle
            graphics:set_color( G, ?BACKGROUND_CIRCLE_COLOR ),
            graphics:fill_oval( G,
                    { -770, 280 },
                    { ?BACKGROUND_CIRCLE_DIAMETER, ?BACKGROUND_CIRCLE_DIAMETER },
                    100
            ),

            % draw the pettles around it
            graphics:set_color( G, color:white() ),
            graphics:draw_image_rotated( G, PetalImg, 89 , 92 , -0.104 ),
            graphics:draw_image_rotated( G, PetalImg, 404, 100,  0.157 ),
            graphics:draw_image_rotated( G, PetalImg, 708, 190,  0.419 )
    end,
    actor:new( Act, Paint ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Batons
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      new_player_baton
%%
%% @doc Creates a new player baton.
%% This baton will track the mouse y location on the display.
%% The image given is the image to use for the baton.
%%
%% @spec new_player_baton( Img::image() ) -> Player::actor()
new_player_baton( Img ) ->
    % player specific logic, the player follows the mouse
    Act = fun( AS, Parent ) ->
            Controls = actor:get_controls( Parent ),
            actor_state:set_y( AS, controls:get_mouse_y(Controls) )
    end,
    new_baton( Img, ?BATON_X, Act ).

%%      new_ai_baton
%%
%% @doc Creates a new baton actor for the ai.
%% This baton will move up and down and the image given is the image to use for
%% the baton.
%%
%% @spec new_ai_baton( Img::image() ) -> Player::actor()
new_ai_baton( Img ) ->
    Act = fun( AS, _Parent ) ->
            DeltaY = actor_state:get( AS, delta_y ),
            Height = actor_state:get_height( AS ),
            
            case is_offscreen(AS) of
                top ->
                    AS2 = actor_state:set_y( AS, Height/2 + 1 ),
                    actor_state:set( AS2, delta_y, -DeltaY );
                bottom ->
                    AS2 = actor_state:set_y( AS, ?HEIGHT-Height/2 - 1 ),
                    actor_state:set( AS2, delta_y, -DeltaY );
                _Other ->
                    actor_state:move( AS, 0, DeltaY )
            end
    end,
    new_baton( Img, ?WIDTH-?BATON_X, Act, [{delta_y, ?AI_BATON_SPEED}] ).

%%      new_baton
%%
%% @doc Creates a new generic baton which can be used by both players or computers.
%% The image, starting x location and the update logic are what needs to be
%% supplied by the caller.
%%
%% @spec new_baton( Img::image(), X::number(), Act::(AS::actor_state(), Parent::world()) -> NewAS ) -> Baton::actor()
new_baton( Img, X, Act ) ->
    new_baton( Img, X, Act, [] ).

%%      new_baton
%%
%% @doc The same as the other new_baton function, but with the ability to set custom properties.
%% @spec new_baton( Img::image(), X::number(), Act::(AS::actor_state(), Parent::world(), Properties::[{Key::atom(), Value::term()}] ) -> NewAS ) -> Baton::actor()
new_baton( Img, X, Act, Properties ) ->
    State = actor_state:new( baton, {X, ?HEIGHT/2}, image:get_size(Img) ),
    actor:new( Act, new_image_paint(Img), actor_state:set(State, Properties) ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Score
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      new_score
%%
%% @doc Creates and returns a new actor scoreboard.
%% This is for recording and displaying the scores.
%% @spec new_score() -> ScoreBoard::actor()
new_score( Display ) ->
    State = actor_state:new(
            score, {?SCORE_X, ?SCORE_Y}, {1,1}, [
                    { scores, {0, 0} },
                    { score_image, new_score_text_image(Display, 0, 0) }
            ] ),
    Act = fun( AS, _Parent ) ->
        receive
            { ?INCREMENT_SCORE, player } -> update_score( AS, Display, 0, 1 );
            { ?INCREMENT_SCORE, ai     } -> update_score( AS, Display, 1, 0 )
        after
            1 -> AS
        end
    end,
    Paint = fun( AS, G ) ->
        ScoreText = actor_state:get( AS, score_image ),
        XY = actor_state:get_xy( AS ),
        graphics:set_color( G, ?SCORE_TEXT_COLOR ),
        graphics:draw_image( G, ScoreText, XY )
    end,
    actor:new( Act, Paint, State ).

%%      update_score
%%
%% @doc Updates the score stored in the actor state, returning a new state with this set.
%% @spec update_score( AS::actor_state(), Display::display(), PlayerIncrement::number(), AIIncrement::number() ) -> NewAS::actor_state()
update_score( AS, Display, PlayerIncrement, AIIncrement ) ->
    {PlayerScore, AIScore} = actor_state:get( AS, scores ),
    NewAS = actor_state:set( AS, scores, {PlayerScore+PlayerIncrement , AIScore+AIIncrement} ),
    % destroy the old score text, and replace it with the new text
    ScoreImage = actor_state:get( AS, score_image ),
    text_image:destroy( ScoreImage ),
    actor_state:set( NewAS, score_image, new_score_text_image(Display, PlayerScore, AIScore) ).

%%      increment_score
%%
%% @doc Messages the score actor stating to update the score.
%% @spec increment_score( Score::actor(), player | ai ) -> ok
increment_score( Score, player ) -> Score ! { ?INCREMENT_SCORE, player };
increment_score( Score, ai     ) -> Score ! { ?INCREMENT_SCORE, ai     }.

%%      new_score_text_image
%%
%% @doc Creates a new image for displaying the score text, all formatted.
%% @spec new_score_text_image( Display::display(), ScorePlayer::number(), ScoreComputer::number() ) -> ScoreImage::image()
new_score_text_image( Display, ScorePlayer, ScoreComputer ) ->
    ScoreText = io_lib:format( "Player: ~w, Computer: ~w", [ScorePlayer, ScoreComputer] ),
    text_image:new( Display, ScoreText ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Ball
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      new_ball
%%
%% @doc Creates and returns an actor for the ball in the game.
%% The Score given is the scoreboard actor for the ball to inform that either
%% the player or computer has scored.
%% 
%% @spec new_ball( Img::image(), Score::actor() ) -> Ball::actor()
new_ball( Img, Score ) ->
    State = actor_state:new(
            ball, {?WIDTH/2, ?HEIGHT/2}, image:get_size(Img), [
                { delta_xy, {?BALL_SPEED, 0} }
            ]
    ),
    Act = fun( AS, Parent ) ->
        SelfAS = actor_state:get_actor( AS ),
        CollFun = fun( OtherAS ) ->
                SelfAS ! { ?BALL_COLLISION_MSG, OtherAS }
        end,
        world:apply_to_intersecting_actor( Parent, actor_state:get_actor(AS), baton, CollFun ),
        
        DeltaXY = receive
            { ?BALL_COLLISION_MSG, OtherAS } -> calculate_delta_xy( OtherAS, AS, ?BALL_SPEED )
        after
            1 -> actor_state:get( AS, delta_xy )
        end,
        
        % check if it has gone off-screen
        NewAS = case is_offscreen(AS) of
            left   ->
                increment_score(Score, player),
                TempAS = actor_state:set_xy( AS, ?WIDTH/2, ?HEIGHT/2 ),
                { DeltaX, DeltaY } = DeltaXY,
                actor_state:set( TempAS, delta_xy, {-DeltaX, DeltaY} );
            right  ->
                increment_score(Score, ai),
                TempAS = actor_state:set_xy( AS, ?WIDTH/2, ?HEIGHT/2 ),
                { DeltaX, DeltaY } = DeltaXY,
                actor_state:set( TempAS, delta_xy, {-DeltaX, DeltaY} );
            top    ->
                { DeltaX, DeltaY } = DeltaXY,
                TempAS = actor_state:set( AS, delta_xy, {DeltaX, -DeltaY} ),
                actor_state:set_y( TempAS, actor_state:get_height(AS)/2 + 1 );
            bottom ->
                { DeltaX, DeltaY } = DeltaXY,
                TempAS = actor_state:set( AS, delta_xy, {DeltaX, -DeltaY} ),
                actor_state:set_y( TempAS, ?HEIGHT - actor_state:get_height(AS)/2 - 1 );
            none   ->
                actor_state:set( AS, delta_xy, DeltaXY )
        end,
        
        actor_state:move( NewAS, actor_state:get(NewAS, delta_xy) )
    end,
    Paint = new_image_paint( Img ),
    actor:new( Act, Paint, State ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Utility Functions
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      is_offscreen
%%
%% @doc Checks if the given actor_state is outside of the screen.
%% An atom is returned describing where it is.
%%
%% @spec is_offscreen( AS::actor_state() ) -> left | right | top | bottom | none
is_offscreen(AS) ->
    {X, Y} = actor_state:get_xy(AS),
    {W, H} = actor_state:get_size(AS),
    if
        (X-W/2) < 0       -> left   ;
        (X+W/2) > ?WIDTH  -> right  ;
        (Y-H/2) < 0       -> top    ;
        (Y+H/2) > ?HEIGHT -> bottom ;
        true              -> none
    end.

%%      calculate_angle
%% 
%% @doc Returns the angle from the first location to the second location.
%% @spec calculate_angle( {FromX::number(), FromY::number()}, {ToX::number(), ToY::number} ) -> Angle::number()
calculate_angle( {FromX, FromY}, {ToX, ToY} ) ->
    math:atan2( ToY-FromY, ToX-FromX ).

%%      calculate_delta_xy
%%
%% @doc Returns a tuple of speed if taken from the first actor_state to the second.
%% This works out the angle first between them and then multiplies against the
%% speed.
%%
%% @spec calculate_delta_xy( FromAS::actor_state(), ToAS::actor_state(), Speed::number() ) -> { DeltaX::number(), DeltaY::number() }
calculate_delta_xy( FromAS, ToAS, Speed ) ->
    XY = actor_state:get_xy( ToAS ),
    FromXY = actor_state:get_xy( FromAS ),
    Angle = calculate_angle( FromXY, XY ),
    { Speed*math:cos(Angle), Speed*math:sin(Angle) }.

%%      new_image_paint
%%
%% @doc Returns a fun for an Actor paint that will paint using the given image.
%% @spec new_image_paint( Img::image() ) -> Paint::(actor_state(), graphics()) -> term()
new_image_paint(Img) ->
    fun( AS, G ) ->
        XY = actor_state:get_xy( AS ),
        graphics:draw_image( G, Img, XY, true )
    end.
