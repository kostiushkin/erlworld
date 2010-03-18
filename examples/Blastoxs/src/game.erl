
%%%
%%%         game
%%%
%%% This is the main game board. It runs both the titles and in-game world as
%%% one state machine.
%%%
%%% For the titles it will check if the player has pressed space and start the
%%% game if they have. Otherwise it is waiting for the end of the in-game
%%% transition.
%%%
%%% When updating the game it waits for asteroid and player destruction messages.
%%% When asteroids are destroyed it will
%%% 

-module( game ).

-include_lib("ErlWorld/include/controls.hrl").
-include("blastox.hrl").

-define( NUM_STARTING_ASTEROIDS, 3   ).
-define( NUM_STARTING_STARS    , 100 ).

-define( MSG_ASTEROID_DEATH, asteroid_death_message    ).
-define( MSG_END_TRANSITION, end_of_transition_message ).
-define( MSG_PLAYER_DEATH  , player_death_message      ).

-define( LOGO_Y_ANGLE_INCREMENT, 0.02 ). % just under 5 degrees, but in radians
-define( LOGO_Y_MAX_OFFSET     , 32   ).

-define( LOGO_X, ?WIDTH/2  ).
-define( LOGO_Y, ?HEIGHT/3 ).

-define( TRANSITION_TIME, 20 ).

-define( START_TEXT, "PRESS SPACE").
-define( DEATH_TEXT, "PRESS SPACE TO PLAY AGAIN").
-define( START_TEXT_Y, (?HEIGHT/4) * 3 ).

-define( DEAD_RESET_COUNT, 30 ).

% the number of frames that the text should appear for
-define( DRAW_MAX_TICKS, 30 ).

-define( SCORE_TEXT, "SCORE: ").
-define( SCORE_PADDING, 4 ).

-export([
        new/1,
        send_asteroid_death/1,
        send_player_death/1,
        send_end_transition/1
]).

%%          new
%%
%% @doc Creates a new game world. The given display is the display which the game is running within.
%% The created game is a World server object.
%% 
%% @spec new( Display::display() ) -> Game::world()
new( Display ) ->
    Img1 = image:new( Display, "./images/asteroids/red.png" ),
    Img2 = image:new( Display, "./images/asteroids/green.png" ),
    Img3 = image:new( Display, "./images/asteroids/blue.png" ),

    PImg1 = image:new( Display, "./images/player.png"    ),
    PImg2 = image:new( Display, "./images/bullet.png"    ),
    PImg3 = image:new( Display, "./images/explosion.png" ),
    
    Logo = image:new( Display, "./images/logo.png" ),
    
    PressStart = text_image:new( Display, ?START_TEXT ),
    DeathText  = text_image:new( Display, ?DEATH_TEXT ),
    
    StartState = add_stars(
            world_state:new(
                    [
                    ],
                    [
                            { score             , 0                     },
                            { score_image       , none                  },
                            { logo_y_angle      , 0                     },
                            { state             , titles                },
                            { transition        , false                 },
                            { asteroid_img_num  , 0                     },
                            { asteroid_imgs     , {Img1, Img2, Img3}    },
                            { player_images     , {PImg1, PImg2, PImg3} },

                            { draw_text_image   , true                  },
                            { draw_ticks        , ?DRAW_MAX_TICKS       },

                            { image_logo        , Logo                  },
                            { image_press_start , PressStart            },
                            { image_death_text  , DeathText             },

                            { is_dead           , false                 },
                            { dead_reset_counter, 0                     },

                            { display           , Display               }
                    ]
            ),
            ?NUM_STARTING_STARS
    ),
    
    % act
    Act = fun( WS, Parent ) ->
        act( WS, Parent, actor_state:get(WS, state) )
    end,

    % paint
    Paint = fun( WS, G ) ->
        graphics:set_color( G, color:white() ),
        paint( WS, G, actor_state:get(WS, state) )
    end,
    
    world:new( Act, Paint, StartState ).

%%          act
%%
%% @doc Updates the game for both it's titles and in game sections.
%% @spec act( WS::world_state(), Parent::pid(), titles | game ) -> NewWS::world_state()
act( WS, Parent, titles ) ->
    IsTransition = actor_state:get( WS, transition ),
    world_state:act_actors(
        increment_draw_ticks(
            increment_logo_y(
                if
                    IsTransition ->
                        receive
                            ?MSG_END_TRANSITION -> reset_player( WS )
                        after
                            1 -> WS
                        end;
                    true ->
                        case controls:is_key_down( actor:get_controls(Parent), ?KEY_SPACE ) of
                            true   -> act_start_transition( WS );
                            _Other -> WS
                        end
                end
            )
        )
    );
act( WS, _Parent, game ) ->
    NewWS = world_state:act_actors(
            act_game( WS, actor_state:get(WS, is_dead) )
    ),
    NewWS.

%%          act_game
%%
%% @doc Updates the game world when it's in it's game state.
%% This can be updated itself in one of two states; when the player is alive and
%% when they are dead. If the player is alive then it waits on inputs that the
%% player or an asteroid has died.
%%
%% When the player is dead this checks for control input to respawn the player.
%%
%% @spec act_game( WS::world_state(), IsDead::boolean() ) -> NewWS::world_state()
act_game( WS, true ) ->
    Count = actor_state:get( WS, dead_reset_counter ),
    NewWS = if
        (Count > 0) ->
            actor_state:set( WS, dead_reset_counter, Count-1 );
        true ->
            Self = actor_state:get_actor(WS),
            case controls:is_key_down( actor:get_controls(Self), ?KEY_SPACE ) of
                true  -> reset_player( WS );
                false -> WS
            end
    end,
    increment_draw_ticks( NewWS );
act_game( WS, false ) ->
    receive
        ?MSG_PLAYER_DEATH   ->
            actor_state:set( WS,
                    [
                        { draw_text_image   , false             },
                        { draw_ticks        , ?DRAW_MAX_TICKS   },
                        { is_dead           , true              },
                        { dead_reset_counter, ?DEAD_RESET_COUNT }
                    ] );
        ?MSG_ASTEROID_DEATH ->
            Score = actor_state:get( WS, score )+1,
            destroy_image( actor_state:get(WS, score_image) ),
            actor_state:set(
                    world_state:act_actors( add_asteroid(WS) ),
                    [
                        { score, Score },
                        { score_image, new_score_image(WS, Score) }
                    ]
            )
    after
        1 -> WS
    end.

%%          new_score_image
%%
%% @doc Returns a new score image to use when drawing the score based on the score given.
%% @spec new_score_image( WS::world_state(), Score::integer() ) -> ScoreImg::image()
new_score_image( WS, Score ) ->
    D = actor_state:get(WS, display),
    RandScore = if
        Score == 0 -> 0;
        true       -> round( rand:random(?RANDOM, 100) )
    end,
    PrintScore = (Score*100) + RandScore,
    Text = io_lib:format( ?SCORE_TEXT ++ "~w", [PrintScore] ),
    text_image:new( D, Text ).

%%          destroy_image
%%
%% @doc Destroys the image given, if it's an image.
%% If it's not an image then none should be given in order to do nothing.
%%
%% @spec destroy_image( none | Img::image() ) -> ok
destroy_image( none ) -> ok;
destroy_image( Img  ) -> image:destroy( Img ).

%%          reset_player
%%
%% @doc Adds a player to the world and resets the game to when it has just started being played.
%% @spec reset_player( WS::world_state() ) -> NewWS::world_state()
reset_player( WS ) ->
    PlayerImgs = actor_state:get( WS, player_images ),
    Player = player:new( PlayerImgs ),
    StartingWS = add_asteroids( remove_asteroids(WS), ?NUM_STARTING_ASTEROIDS ),
    NewWS  = actor_state:set( StartingWS, [
            { score  , 0     },
            { score_image, new_score_image(WS, 0) },
            { state  , game  },
            { is_dead, false }
    ] ),
    world_state:set_paint_order(
            world_state:add_actor(
                    NewWS,
                    Player
            ),
            Player,
            ?PAINT_PLAYER
    ).

%%          paint
%%
%% @doc The painting function for the game world.
%% This runs in one of two states. It painting as the titles then it will paint
%% the logo and the press space image. If it's in the game state then it will
%% paint the score and the reset player text (when the player is dead).
%%
%% @spec paint( WS::world_state(), G::graphics(), titles | game ) -> ok
paint( WS, G, titles ) ->
    X = ?LOGO_X,
    Y = ?LOGO_Y + ?LOGO_Y_MAX_OFFSET*math:sin( get_logo_y(WS) ),
    Logo = actor_state:get( WS, image_logo ),
    
    graphics:draw_image( G, Logo, X, Y, true ),
    case actor_state:get( WS, draw_text_image ) of
        true ->
            PressStart = actor_state:get( WS, image_press_start ),
            graphics:draw_image(
                G, PressStart,
                ?WIDTH/ 2, ?START_TEXT_Y,
                true );
        _Else -> ok
    end,
    world_state:paint_actors( WS, G );
paint( WS, G, game ) ->
    world_state:paint_actors( WS, G ),
    Img = actor_state:get( WS, score_image ),
    paint_reset_player( WS, G, actor_state:get(WS, is_dead) ),
    paint_score( G, Img, ?SCORE_PADDING, ?SCORE_PADDING ).

%%          paint_score
%%
%% @doc Paints the score image to the graphics given, if it is given.
%% If the none atom is provided then nothing will happen.
%%
%% @spec paint_score( G::graphics(), none | Img::image(), X::number(), Y::number() ) -> ok
paint_score( _G, none, _X, _Y ) -> ok;
paint_score(  G, Img ,  X,  Y ) -> graphics:draw_image( G, Img, X, Y, false ).

%%          paint_reset_player
%%
%% @doc If the player is dead then the player reset text is painted. Otherwise nothing happens.
%% @spec paint_reset_player( WS::world_state(), G::graphics(), IsPlayerDead::boolean() ) -> ok
paint_reset_player( _WS, _G, false ) -> ok;
paint_reset_player(  WS,  G, true  ) ->
    case actor_state:get( WS, draw_text_image ) of
        true ->
            ResetText = actor_state:get( WS, image_death_text ),
            graphics:draw_image(
                G, ResetText,
                ?WIDTH/ 2, ?START_TEXT_Y,
                true );
        _Else -> ok
    end.

%%          increment_draw_ticks
%%
%% @doc Updates the number of frames that the logo will appear for.
%% When the number of ticks reaches zero, then it will be reset back up to the
%% maximum number and the draw_text_iamge will be flipped between true and false.
%%
%% @spec increment_draw_ticks( WS::world_state() ) -> NewWS::world_state()
increment_draw_ticks(WS) -> increment_draw_ticks( WS, actor_state:get(WS, draw_ticks) ).

%%          increment_draw_ticks
%%
%% @doc The same as the other increment_draw_ticks function, only this uses the
%% Ticks parameter as the number of ticks in the world_state.
%%
%% @spec increment_draw_ticks( WS::world_state(), Ticks::number() ) -> NewWS::world_state()
increment_draw_ticks(WS, Ticks) when (Ticks =< 0) ->
    actor_state:set( WS, [
        { draw_ticks, ?DRAW_MAX_TICKS },
        { draw_text_image, not actor_state:get(WS, draw_text_image) }
    ] );
increment_draw_ticks(WS, Ticks) -> actor_state:set( WS, draw_ticks, Ticks-1 ).

%%          increment_logo_y
%%
%% @doc Increments the y offset when drawing the logo.
%% This allows the logo to move up and down along a sin wave.
%%
%% @spec increment_logo_y( WS::world_state() ) -> NewWS::world_state()
increment_logo_y(WS) -> set_logo_y(WS, get_logo_y(WS)+?LOGO_Y_ANGLE_INCREMENT).

%%          set_logo_y
%%
%% @doc Sets the y angle used for calculating the y offset for drawing the logo.
%% This value is in radians.
%%
%% @spec set_logo_y( WS::world_state(), NewY::number() ) -> NewWS::world_state()
set_logo_y(WS, NewY) -> actor_state:set(WS, logo_y_angle, NewY).

%%          get_logo_y
%%
%% @doc Returns the y angle offset for drawing the logo. This value is in radians.
%% @spec get_logo_y( WS::world_state() ) -> NewWS::world_state()
get_logo_y(WS) -> actor_state:get(WS, logo_y_angle).

%%          act_start_transition
%%
%% @doc Adds a new transition actor to the world_state.
%% @spec act_start_transition( WS::world_state() ) -> NewWS::world_state()
act_start_transition( WS ) ->
    T = transition:new( ?TRANSITION_TIME ),
    NewWS = world_state:add_actor( WS, T ),
    actor_state:set(
        world_state:set_paint_order( NewWS, T, ?PAINT_TRANSITIONS ),
        transition, true ).

%%          remove_asteroids
%%
%% @doc Removes all asteroids from the world_state.
%% @spec remove_asteroids( WS::world_state() ) -> NewWS::world_state()
remove_asteroids( WS ) -> remove_asteroids( actor_state:get_actor(WS), world_state:get_actors(WS) ), WS.

%%          remove_asteroids
%%
%% @doc Removes all of the asteroids in the list given from the world given.
%% @spec remove_asteroids( Parent::world(), Asteroids::[actor()] ) -> ok
remove_asteroids( _Parent, [] ) -> ok;
remove_asteroids( Parent, [A | As] ) ->
    case actor:get_name(A) of
        ?NAME_ASTEROID -> world:remove_actor( Parent, A );
        _Else -> ok
    end,
    remove_asteroids( Parent, As ).

%%          add_asteroids
%%
%% @doc Adds the given number of asteroids to the world_state.
%% @spec add_asteroids( WS::world_state(), Num::number() ) -> NewWS::world_state()
add_asteroids( WS, Num ) when (Num =< 0) -> WS;
add_asteroids( WS, Num )                 -> add_asteroids( add_asteroid(WS), Num-1 ).

%%          add_asteroid
%%
%% @doc Adds a new asteroid to the given world_state.
%% @spec add_asteroid( WS::world_state() ) -> NewWS::world_state()
add_asteroid( WS ) ->
    { AsteroidNum, Img } = asteroid_img(
                    actor_state:get(WS, asteroid_img_num),
                    actor_state:get(WS, asteroid_imgs   ) ),
    actor_state:set(
            world_state:add_actor( WS, asteroid:new(Img) ),
            asteroid_img_num,
            AsteroidNum
    ).

%%          add_stars
%%
%% @doc Adds the stated number of stars to the world_state given.
%% @spec add_stars( WS::world_state(), Num::number() ) -> NewWS::world_state()
add_stars( WS, Num ) when (Num =< 0) -> WS;
add_stars( WS, Num )                 -> add_stars( add_star(WS), Num-1 ).

%%          add_star
%%
%% @doc Adds a new star to the world_state.
%% @spec add_star( WS::world_state() ) -> NewWS::world_state()
add_star( WS ) ->
    Star = star:new(),
    world_state:set_paint_order(
        world_state:add_actor( WS, Star ),
        Star,
        ?PAINT_STAR ).

asteroid_img( 0, {Img1, _Img2, _Img3} ) -> { 1, Img1 };
asteroid_img( 1, {_Img1, Img2, _Img3} ) -> { 2, Img2 };
asteroid_img( 2, {_Img1, _Img2, Img3} ) -> { 0, Img3 }.

%%          send_asteroid_death
%%
%% @doc Sends a message to the game that an asteroid has been destroyed.
%% @spec send_asteroid_death( Game::actor() ) -> ok
send_asteroid_death(Game) -> Game ! ?MSG_ASTEROID_DEATH.

%%          send_player_death
%%
%% @doc Sends a message to the game that the player has died.
%% @spec send_player_death( Game::actor() ) -> ok
send_player_death(Game)   -> Game ! ?MSG_PLAYER_DEATH.

%%          send_end_transition
%%
%% @doc Sends a message to the game that the intro animation has ended.
%% @spec send_end_transition( Game::actor() ) -> ok
send_end_transition(Game) -> Game ! ?MSG_END_TRANSITION.
