
%%%
%%%             Image
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This is for handling image loading and defines the structure of an image.
%%% 
%%% It offers functions for also retrieving the properties of an image, such as
%%% it's size.
%%% 
%%% Internally this image actually gets the given graphics device to create the
%%% image. This is because only the graphics' process is able to safely make the
%%% WX and OpenGL calls needed to create the image.
%%%

-module(image).
-author("Joseph Lenton").

-include("image.hrl").

-export([
        new/2,

        get_size/1, get_width/1, get_height/1,
        destroy/1
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%              new
%%
%% @doc Creates a new Image loaded from the given FileName.
%% The given display object is used to create the image, which the image will be
%% associated with.
%%
%% @spec new(Display::display(), FileName::string()) -> Image::image()
new(G, FileName) -> data_server:request( G, ?IMAGE_NEW_IMAGE, FileName ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%              get_size
%%
%% @doc Returns the size of the image as it should appear on the screen.
%% @spec get_size(Image::image()) -> {Width::integer(), Height::integer()}
get_size(#img{width=Width, height=Height}) -> {Width, Height}.

%%              get_width
%%
%% @doc The width of this image in pixels.
%% @spec get_width(Image::image()) -> Width::integer()
get_width(#img{width=Width}) -> Width.

%%              get_height
%%
%% @doc The height of this image in pixels.
%% @spec get_height(Image::image()) -> Height::integer()
get_height(#img{height=Height}) -> Height.

%%              destroy
%%
%% @doc Internally unloads this image from the graphics card.
%% After calling this function, do not use or destroy this image. Doing so is
%% unsafe and will cause unknown errors.
%%
%% @spec destroy( Image::image() ) -> ok
destroy( Image ) -> data_server:request( Image#img.destroyer, ?IMAGE_DESTROY_IMAGE, Image ).