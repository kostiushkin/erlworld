
%%%             Display
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This is essentially a window that the framework will use for drawing and
%%% checking for controls.
%%%
%%% You should create only one of these and then use it with the mainloop.
%%% 

-module( display ).
-author("Joseph Lenton").

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-include("display.hrl").
-include("graphics.hrl").
-include("image.hrl").
-include("text_image.hrl").

-define( DEFAULT_TITLE, "ErlWorld" ).

-export([new/2, new/3, new/4]).

% drawing
-export([
        paint_actor/2,
        get_size/1, get_width/1, get_height/1,
        get_controls/1
]).

-record(displayContext, {
        size,
        graphics,
        controls
}).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          new
%%
%% @doc This is exactly the same as the other new function, however this will use the default title.
%% @spec new( Width::integer(), Height::integer() ) -> Display::display()
new( Width, Height ) -> new( Width, Height, ?DEFAULT_TITLE ).

%%          new
%% 
%% @doc Creates a new Display of the given size and with the given title.
%% Note that the width and height are the size of the graphics area inside of
%% the window, not the size of the window itself.
%%
%% By default ErlWorld will shutdown the erlang runtime when this display is
%% closed.
%% 
%% @spec new( Width::integer(), Height::integer(), Title::string() ) -> Display::display()
new(Width, Height, Title) ->
    new( Width, Height, Title, fun() -> halt() end ).

%%          new
%%
%% @doc This new creates a new Display of the given Width and Height with the given Title.
%% It will also run the given OnCloseFun when the display is closed. The result
%% of this fun is ignored and it should have no parameters.
%%
%% @spec new( Width::integer(), Height::integer(), Title::string(), OnCloseFun::() -> term() ) -> Display::display()
new( Width, Height, Title, OnCloseFun ) ->
    data_server:new_in_pid(
            fun( Message,  Params,  State) -> on_message( Message, Params, State ) end,
            fun( Message,  Params,  State) -> on_request( Message, Params, State ) end,
            fun() ->
                CHandler = controls_handler:new(),
                #displayContext{
                        graphics = graphics:new( self(), {Width, Height}, Title, OnCloseFun, CHandler ),
                        size     = {Width, Height},
                        controls = CHandler
                }
            end
    ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Message Handling
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

on_message(?DISPLAY_PAINT_ACTOR, Parms, State) ->
    paint_actor_inner( State, Parms ),
    State.

%%          on_request
%%
%% @doc These deal with the incomming request replies to the server.
on_request(?DISPLAY_GET_CONTROLS, _Params, State) ->
    { State, controls_handler:get_controls( State#displayContext.controls ) };
on_request(?DISPLAY_GET_SIZE_MESSAGE, _Params, State) ->
    { State, get_size_inner(State) };
on_request(?IMAGE_NEW_IMAGE, FileName, State) ->
    { State, data_server:request(State#displayContext.graphics, ?IMAGE_NEW_IMAGE, FileName) };
on_request(?TEXT_NEW_IMAGE, Params, State) ->
    { State, data_server:request(State#displayContext.graphics, ?TEXT_NEW_IMAGE, Params) };
on_request(?IMAGE_DESTROY_IMAGE, Image, State) ->
    { State, data_server:request(State#displayContext.graphics, ?IMAGE_DESTROY_IMAGE, Image) }.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Private
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          get_size_inner
%%
%% @doc Retrieves the canvas size in the Displays process.
%% @spec get_size_inner( Display::display() ) -> {Width::integer(), Height::integer()}
get_size_inner( Display ) -> Display#displayContext.size.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          paint_actor
%%
%% @doc This paints the given Actor to this Display.
%% It will block until painting has finished.
%%
%% Typically you don't ever need to call this. It is called for you by the
%% mainloop.
%% 
%% @spec paint_actor( Display::display(), Actor::actor() ) -> ok
paint_actor( Display, Actor ) ->
    data_server:send( Display, ?DISPLAY_PAINT_ACTOR, {Actor, self()} ),
    receive
        ?DISPLAY_RECEIVE_GRAPHICS_FINISHED -> ok
    end.

%%          paint_actor_inner
%%
%% @doc This is the function that does the painting in the Display's process.
%% @spec paint_actor_inner( Display::display(), Actor::actor() ) -> ok
paint_actor_inner(Display, Actor) ->
    data_server:send( Display#displayContext.graphics, ?GRAPHICS_PAINT_ACTOR, Actor ),
    Display.

%%          get_size
%%
%% @doc Return a tuple in the format {Width, Height} depicting the size of the canvas.
%% @spec get_size( Display::display() ) -> {Width::integer(), Height::integer()}
get_size( Display ) -> data_server:request( Display, ?DISPLAY_GET_SIZE_MESSAGE ).

%%          get_width
%%
%% @doc Returns the width of the graphics area of the Display.
%% @spec get_width( Display::display() ) -> Width::integer()
get_width( Display ) ->
    {Width, _Height} = get_size( Display ),
    Width.

%%          get_height
%%
%% @doc Returns the height of the graphics area of the Display.
%% @spec get_height( Display::display() ) -> Height::integer()
get_height( Display ) ->
    {_Width, Height} = get_size( Display ),
    Height.

%%          get_controls
%%
%% @doc Returns a snapshot of the current state of the controls input.
%% @spec get_controls( Display::display() ) -> Controls::controls()
get_controls(Display) ->
    data_server:request( Display, ?DISPLAY_GET_CONTROLS ).
