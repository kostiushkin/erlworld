
%%%
%%%             Color
%%%
%%% @author Joseph Lenton
%%% @doc
%%% Defines how to create colours and how to retrieve the values from colours.
%%% Colours are stored as a tuple in the form {red, green, blue, alpha}. All
%%% components are floats.
%%%
%%% By using this file it allows the user to abstract away how they care about
%%% colours.
%%%
%%% It also contains common colours for you to use.
%%%

-module(color).

-export([ new/3, new/4 ]).
-export([ get_red/1, get_green/1, get_blue/1, get_alpha/1 ]).
-export([
        white/0, grey/0, black/0,
        red/0, green/0, blue/0,
        yellow/0, orange/0,
        pink/0, purple/0
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Constructors
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          new
%%
%% @doc Creates and returns a new Color object with full alpha.
%% @spec new( Red::float(), Green::float(), Blue::float() ) -> Color::color()
new(Red, Green, Blue)        -> new( Red, Green, Blue, 1.0   ).

%%          new
%%
%% @doc Creates and returns a new Color object.
%% @spec new( Red::float(), Green::float(), Blue::float(), Alpha::float() ) -> Color::color()
new(Red, Green, Blue, Alpha) ->    { Red, Green, Blue, Alpha }.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Interactions
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          get_red
%%
%% @doc Returns the red component from the given colour.
%% @spec get_red( Color::color() ) -> Red::float()
get_red   ({  Red, _green, _blue, _alpha }) -> Red.

%%          get_green
%%
%% @doc Returns the green component from the given colour.
%% @spec get_green( Color::color() ) -> Green::float()
get_green ({ _red,  Green, _blue, _alpha }) -> Green.

%%          get_blue
%%
%% @doc Returns the blue component from the given colour.
%% @spec get_blue( Color::color() ) -> Blue::float()
get_blue  ({ _red, _green,  Blue, _alpha }) -> Blue.

%%          get_alpha
%%
%% @doc Returns the alpha component from the given colour.
%% @spec get_alpha( Color::color() ) -> Alpha::float()
get_alpha ({ _red, _green, _blue,  Alpha }) -> Alpha.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Common Colours
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          white
%%
%% @doc A predefined color, white.
%% @spec white() -> White::color()
white()     -> new(1.0, 1.0, 1.0).

%%          grey
%%
%% @doc A predefined color, grey.
%% @spec grey() -> Grey::color()
grey()      -> new(0.5, 0.5, 0.5).

%%          black
%%
%% @doc A predefined color, black.
%% @spec black() -> Black::color()
black()     -> new(0.0, 0.0, 0.0).

%%          red
%%
%% @doc A predefined color, red.
%% @spec red() -> Red::color()
red()       -> new(1.0, 0.0, 0.0).

%%          green
%%
%% @doc A predefined color, green.
%% @spec green() -> Green::color()
green()     -> new(0.0, 1.0, 0.0).

%%          blue
%%
%% @doc A predefined color, blue.
%% @spec blue() -> Blue::color()
blue()      -> new(0.0, 0.0, 1.0).

%%          yellow
%%
%% @doc A predefined color, yellow.
%% @spec yellow() -> Yellow::color()
yellow()    -> new(1.0, 1.0, 0.0).

%%          orange
%%
%% @doc A predefined color, orange.
%% @spec orange() -> Orange::color()
orange()    -> new(1.0, 0.5, 0.0).

%%          pink
%%
%% @doc A predefined color, pink.
%% @spec pink() -> Pink::color()
pink()      -> new(1.0, 0.75, 0.796).

%%          purple
%%
%% @doc A predefined color, purple.
%% @spec purple() -> Purple::color()
purple()    -> new(0.5, 0.0, 0.5).
