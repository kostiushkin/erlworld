
%%%         text_image
%%%
%%% When using the draw_text method the text is generated on the fly before it
%%% is drawn. This can be very slow when drawing large amounts of text.
%%%
%%% This module allows you to do that generation once which is stored as an
%%% image. The image can then be drawn just like a normal image in order to
%%% display the text.
%%%
%%% New objects created with this module are fully compatible with the functions
%%% in the image module.
%%%

-module( text_image ).
-author("Joseph Lenton").

-include("text_image.hrl").

-export([
    new/2, new/3
]).

% wrapped from the image module
-export([
        get_size/1, get_width/1, get_height/1,
        destroy/1
]).

%%          new
%%
%% @doc Creates a new textual image using the default font.
%% @spec new( Display::display(), Text::string() ) -> text_image()
new( Display, Text ) -> new( Display, Text, none ).

%%          new
%%
%% @doc Creates an image displaying the given text using the given font.
%% The font should be a wxFont, and it's the callers responsibility to destroy
%% it after calling this function. For the default font suplly the atom 'none'.
%%
%% @spec new( Display::display(), Text::string(), WXFont::wxFont() | none ) -> text_image()
new( Display, Text, WXFont ) -> data_server:request( Display, ?TEXT_NEW_IMAGE, {Text, WXFont} ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%              get_size
%%
%% @equiv image:get_size( Image )
get_size( Image ) -> image:get_size( Image ).

%%              get_width
%%
%% @equiv image:get_width( Image )
get_width( Image ) -> image:get_width( Image ).

%%              get_height
%%
%% @equiv image:get_height( Image )
get_height( Image ) -> image:get_height( Image ).

%%              destroy
%%
%% @equiv image:destroy( Image )
destroy( Image ) -> image:destroy( Image ).