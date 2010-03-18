
%%%
%%%         blastox
%%%
%%% @doc These are game wide settings.
%%% Includes display settings, actor names and paint order numbers.
%%%

% display settings, size and title
-define( WIDTH , 640 ).
-define( HEIGHT, 480 ).
-define( TITLE , "blastox" ).

% names for the different actor_states in the game
-define( NAME_BULLET, bullet ).
-define( NAME_PLAYER, player ).
-define( NAME_ASTEROID, asteroid ).
-define( NAME_PLAYER_DEATH, player_death ).
-define( NAME_STAR, star ).

% painting orders for the different types of actors
-define( PAINT_TRANSITIONS, 1000 ).
-define( PAINT_STAR       , -1   ).
-define( PAINT_PLAYER     , 1    ).

% a global atom for a random number generator
-define( RANDOM, random_number_server ).
