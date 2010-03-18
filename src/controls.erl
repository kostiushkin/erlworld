
%%%             controls
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This is a module for defining how control information is accessed. You
%%% should never be concerned about creating this yourself, instead it will
%%% always be created for you and you just use this module for accessing the
%%% values within it.
%%%
%%% This is essentially a collection with stored information about the current
%%% state of the controls. You just calls this modules functions passing in the
%%% controls asking about the state of a particular mouse button, key or mouse
%%% location.
%%% 

-module( controls ).
-author("Joseph Lenton").
-include("controls_handler.hrl").

-export([
    get_mouse_xy/1,
    get_mouse_x/1,
    get_mouse_y/1,

    is_mouse_down/2,
    is_key_down/2
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%             Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%             Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      get_mouse_xy
%%
%% @doc This returns the location of the mouse in pixels in the frame.
%% 0,0 is the top left corner and this extends across to the right and down to the bottom.
%% @spec get_mouse_xy( Controls::controls() ) -> { X::integer(), Y::integer() }
get_mouse_xy( Controls ) ->
    Controls#control_state.mouse_position.

%%      get_mouse_x
%% 
%% @doc This returns the horizontal X location of the mouse.
%% @spec get_mouse_x( Controls::controls() ) -> X::integer()
get_mouse_x( Controls ) ->
    {X, _Y} = get_mouse_xy(Controls),
    X.

%%      get_mouse_y
%%
%% @doc This returns the vertical Y location of the mouse.
%% @spec get_mouse_y( Controls::controls() ) -> Y::integer()
get_mouse_y( Controls ) ->
    {_X, Y} = get_mouse_xy(Controls),
    Y.

%%      is_mouse_down
%%
%% @doc This checks if the given button is down, and return true or false accordingly.
%% The button supplied can be either one of the macros defined in the
%% controls.hrl, or the atoms: left, right and middle (for the corresponding
%% mouse button). No other mouse buttons can be checked for.
%% 
%% @spec is_mouse_down( Controls::controls(), Button::atom() ) -> bool()
is_mouse_down( Controls, Button ) ->
    dict:is_key( Button, Controls#control_state.mouse_down ).

%%      is_key_down
%%
%% @doc This is for checking if a particular key is currently pressed, or not.
%% For checking if a key is down you must supply the key as a character, for
%% example to check for a you give $a
%%
%% Not all keys have a character, like the arrows, so for these you can supply
%% the macros defined in controls.hrl. These all start with 'KEY_', such as
%% 'KEY_ENTER' for the enter key.
%%
%% The given key is an integer, but don't let this confuse you. In Erlang all
%% characters are really Integers.
%% 
%% @spec is_key_down( Controls::controls(), Key::integer() ) -> bool()
is_key_down( Controls, Key ) ->
    dict:is_key( Key, Controls#control_state.keys_down ).
