
%%%
%%%             Graphics
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This is a high level abstraction above the underlying graphics device. It
%%% allows the user to draw to the screen. You should never be creating this
%%% yourself, instead the MainLoop will retreive this automatically from the
%%% Display when it paints and will then automatically pass it to the root Actor
%%% inside of itself.
%%% 
%%% Drawing commands are typically split into two variations, draw and fill. The
%%% draw will create the outline of the stated shape. The fill command will draw
%%% the shape filled.
%%% 
%%% When drawing the 0,0 co-ordinate is the top left corner. Increasing the x
%%% value will move across the x-axis to the right. Increasing the y co-ordinate
%%% will move the location down the screen. So the location of width,height is
%%% located at the bottom left corner of the display.
%%%
%%% Unless otherwise stated, all drawing commands draw where the co-ordinates
%%% given are the top-left corner of what is being draw (i.e. oval, image,
%%% rectangle).
%%%
%%% With many of these functions there are also many different versions, to allow
%%% the user to choose if they are passing in tupled and non-tupled values. For
%%% the duplicate tupled versions of functions the documentation is typically
%%% left brief because it's the same as one of the equivalent functions.
%%%
%%% You are also free to draw outside of the graphics area. It just won't get
%%% shown on the display (because it's outside).
%%% 

-module(graphics).
-author("Joseph Lenton").

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-include("display.hrl").
-include("graphics.hrl").
-include("image.hrl").
-include("text_image.hrl").
-include("controls_handler.hrl").

% constructors
-export([ new/5 ]).
% accessors / mutators
-export([
        set_color/2, set_color/4, set_color/5,
        get_color/1
]).
-export([
        set_clear_color/2, set_clear_color/4, set_clear_color/5,
        get_clear_color/1
]).
-export([
        get_size/1, get_width/1, get_height/1
]).
% drawing
-export([
        fill/1,
        draw_rect/2, draw_rect/3, draw_rect/5,
        fill_rect/2, fill_rect/3, fill_rect/5,
        draw_line/3, draw_line/5,
        draw_oval/2, draw_oval/3, draw_oval/4, draw_oval/5, draw_oval/6,
        fill_oval/2, fill_oval/3, fill_oval/4, fill_oval/5, fill_oval/6,
        draw_point/2, draw_point/3,
        draw_polygon/2, draw_polygon/3, draw_polygon/4,
        fill_polygon/2, fill_polygon/3, fill_polygon/4,
        draw_image/3, draw_image/4, draw_image/5, draw_image/6, draw_image/7,
        draw_image_rotated/4, draw_image_rotated/5, draw_image_rotated/7,
        draw_text/3, draw_text/4, draw_text/5
    ]).

-record(glContext, {
        frame,          % the window which the glCanvas is inside of
        gl,             % the OpenGL canvas where the OpenGL calls get drawn too
        display,        % the companion display process
        size            % the size of the OpenGL canvas (not the frame)
}).

-define( GRAPHICS_COLOR, graphics_dict_color ).
-define( GRAPHICS_CLEAR_COLOR, graphics_dict_clear_color ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      new
%%
%% @doc Internal API function for creating a graphics object.
%% This should never be needed to be called by the user.
%%
%% The graphics returned is a process id to the data server containing the true
%% display and opengl context.
%%
%% The size is the size of the canvas inside of the display, not the size of the
%% display (the display will be slightly bigger). The title will appear in the
%% top of the display and the ControlsHandler is for storing all of the controls
%% inputs.
%%
%% @spec new( DisplayPID::pid(), Size, Title::string(), OnCloseFun::() -> term(), ControlsHandler::controls_handler() ) -> Graphics::pid()
%% where Size = { Width::int(), Height::int() }
new( DisplayPID, Size, Title, OnCloseFun, ControlsHandler ) ->
    data_server:new_in_pid(
            fun( Message, Params, State) -> on_message( Message, Params, State ) end,
            fun( Message, Params, State) -> on_request( Message, Params, State ) end,
            fun() -> new_context(DisplayPID, Size, Title, OnCloseFun, ControlsHandler) end
    ).

%%      new_context
%%
%% @doc This is the function that creates the display and it's internal OpenGL screen.
%%
%% @spec new_context( DisplayPID::pid(), {Width::int(), Height::int()}, Title::string(), OnCloseFun::() -> term(), CHandler::controls_handler() ) -> G::glContext()
new_context(DisplayPID, {Width, Height}, Title, OnCloseFun, CHandler) ->
    WX = wx:new(),
    {Frame, GL} = wx:batch(
            fun() ->
                init_frame(WX, Width, Height, Title, OnCloseFun, CHandler)
            end ),
    G = #glContext{
            frame       = Frame,
            gl          = GL,
            display     = DisplayPID,
            size        = {Width, Height}
    },
    set_color( G, color:white() ),
    set_clear_color( G, color:black() ),
    G.

%%      createWindow
%%
%% @doc A helper function for the initialize method, creates the display.
%% It returns the frame with an internal window that can be drawn too.
%%
%% @spec init_frame( WX::wx(), Width::int(), Height::int(), Title::string(), OnCloseFun::() -> term(), CHandler::controls_handler() ) -> { Frame::wxFrame(), GL::wxGLCanvas() }
init_frame(WX, Width, Height, Title, OnCloseFun, CHandler) ->
    Frame = wxFrame:new( WX, 1, Title, [] ),
    Size = {Width, Height},
    % create GL Canvas
    GLAttrs = [
            {size, Size},
            {attribList, [?WX_GL_RGBA, ?WX_GL_DOUBLEBUFFER, 0]}
    ],
    GL = wxGLCanvas:new(Frame, GLAttrs),
    
    % on close
    wxWindow:connect( Frame, close_window, [{ callback,
            fun( _WXEvent, _WXObject ) ->
                OnCloseFun()
            end
    }]),
    
    % Mouse Movement Callback
    wxWindow:connect(GL, motion, [{ callback,
            fun( #wx{ event=#wxMouse{ x=X, y=Y } }, _WXObject ) ->
                controls_handler:set_mouse_location( CHandler, {X, Y} )
            end
    }]),
    % Mouse Down Callback
    wxWindow:connect(GL, left_down, [{ callback,
            fun( _WXEvent, _WXObject ) ->
                controls_handler:set_mouse_down( CHandler, ?MOUSE_LEFT )
            end
    }]),
    wxWindow:connect(GL, right_down, [{ callback,
            fun( _WXEvent, _WXObject ) ->
                controls_handler:set_mouse_down( CHandler, ?MOUSE_RIGHT )
            end
    }]),
    wxWindow:connect(GL, middle_down, [{ callback,
            fun( _WXEvent, _WXObject ) ->
                controls_handler:set_mouse_down( CHandler, ?MOUSE_MIDDLE )
            end
    }]),
    % Mouse Up Callback
    wxWindow:connect(GL, left_up, [{ callback,
            fun( _WXEvent, _WXObject ) ->
                controls_handler:set_mouse_up( CHandler, ?MOUSE_LEFT )
            end
    }]),
    wxWindow:connect(GL, right_up, [{ callback,
            fun( _WXEvent, _WXObject ) ->
                controls_handler:set_mouse_up( CHandler, ?MOUSE_RIGHT )
            end
    }]),
    wxWindow:connect(GL, middle_up, [{ callback,
            fun( _WXEvent, _WXObject ) ->
                controls_handler:set_mouse_up( CHandler, ?MOUSE_MIDDLE )
            end
    }]),
    % Key Callback Listeners
    KeyUpFunc =
            fun( #wx{ event=#wxKey{ keyCode=Key } }, _WxEvent ) ->
                controls_handler:set_key_up( CHandler, Key )
            end,
    KeyDownFunc =
            fun( #wx{ event=#wxKey{ keyCode=Key } }, WxEvent ) ->
                controls_handler:set_key_down( CHandler, Key ),
                wxEvent:skip( WxEvent )
            end,
    KeyCharFunc =
            fun( #wx{ event=#wxKey{ keyCode=Key } }, _WxEvent ) ->
                controls_handler:set_key_char( CHandler, Key )
            end,
    
    % We need to hook into both frame and canvas,
    % as sometimes keys are sent to one and sometimes to the other.
    wxWindow:connect( Frame, key_up,    [{ callback, KeyUpFunc   }] ),
    wxWindow:connect( Frame, char,      [{ callback, KeyCharFunc }] ),
    wxWindow:connect( Frame, key_down,  [{ callback, KeyDownFunc }] ),
    wxWindow:connect( GL   , key_up,    [{ callback, KeyUpFunc   }] ),
    wxWindow:connect( GL   , char,      [{ callback, KeyCharFunc }] ),
    wxWindow:connect( GL   , key_down,  [{ callback, KeyDownFunc }] ),
    
    % sizing
    set_comp_size(GL, Size),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:setMinSize(MainSz, Size),
    wxSizer:fit(MainSz, Frame),
    
    % disables frame resize
    set_comp_size( Frame, wxWindow:getSize(Frame) ),

    % final display code
    wxWindow:show(Frame),
    wxWindow:setFocus(Frame),
    wxGLCanvas:setCurrent(GL),
    init_gl(GL),
    
    wxWindow:raise(Frame),
    
    % return
    {Frame, GL}.

%%      init_gl
%%
%% @doc This sets up the OpenGL specific code for the OpenGL context.
%% Namely this is setting up the viewpoint so that it uses pixels and the types
%% of alpha blending and 2d support.
%%
%% init_gl( GL::wxGLCanvas() ) -> ok
init_gl(GL) ->
    {Width, Height} = wxWindow:getClientSize(GL),
    
    gl:viewport(0, 0, Width, Height),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:ortho(0, Width, Height, 0, -1, 1),
    
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:clearColor(0.0, 0.0, 0.0, 1.0),

    gl:enable(?GL_BLEND),
    gl:enable(?GL_TEXTURE_2D),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:shadeModel(?GL_FLAT),
    
    ok.

%%      set_comp_size
%%
%% @doc Given a component this will set it's  min, max and standard size to the given size.
%% The component given should be a sub-class of wxWindow.
%%
%% set_comp_size( Comp::wxWindow(), Size::{ Width::int(), Height::int() } -> ok
set_comp_size(Comp, Size) ->
    wxWindow:setMinSize(Comp, Size),
    wxWindow:setMaxSize(Comp, Size),
    wxWindow:setSize(Comp, Size),
    ok.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Message Handling
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      on_message
%%
%% @doc Deals with the paint actor message, informing this to paint the given actor.
%%
%% @spec on_message( Message::atom(), Actor::actor(), State::glContext() ) -> NewState::glContext()
on_message(?GRAPHICS_PAINT_ACTOR, {Actor, DonePID}, State) ->
    wx:batch( fun() ->
        gl:clear( ?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT ),
        actor:paint( Actor, State ),
        wxGLCanvas:swapBuffers( State#glContext.gl ),
        DonePID ! ?DISPLAY_RECEIVE_GRAPHICS_FINISHED
    end ),
    State.

%%      on_request
%%
%% @doc The Graphics request handling code, this deals with creating new images.
%% @spec on_request( Message::atom(), FileName::string(), State::glContext() ) -> { NewState::glContext(), Img::image() }
on_request(?IMAGE_NEW_IMAGE, FileName, State) ->
    { State, new_image_inner(FileName) };
on_request(?TEXT_NEW_IMAGE, {Text, WXFont}, State) ->
    { State, new_text_image_inner(Text, WXFont) };
on_request(?IMAGE_DESTROY_IMAGE, Image, State) ->
    { State, destroy_image(Image) }.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Drawing Commands
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      fill
%%
%% @doc Cleares the given Graphics object with it's currently set draw colour.
%% @spec fill( G::graphics() ) -> ok
fill( G ) ->
    { Width, Height } = G#glContext.size,
    fill_rect( G, {0, 0, Width, Height} ),
    ok.

%%      draw_line
%%
%% @doc Draws a Line to the given G from the start position to the end position.
%% This is the tuple version that can take co-ordinates as tuples.
%% 
%% @spec( G::graphics, {X1::float(), Y1::float()}, {X2::float(), Y2::float()} ) -> ok
draw_line( G, {X1, Y1}, {X2, Y2} ) -> draw_line( G, X1, Y1, X2, Y2 ).

%%      draw_line
%%
%% @doc Draws a Line to the given G from the start position to the end position.
%% @spec( G::graphics, X1::float(), Y1::float(), X2::float(), Y2::float() ) -> ok
draw_line(_G, X1, Y1, X2, Y2 ) ->
    gl:'begin'(?GL_LINES),
    gl:vertex2d(X1, Y1),
    gl:vertex2d(X2, Y2),
    gl:'end'().

%%      draw_rect
%%
%% @doc Draws the outline of a rectangle.
%% The location given is the top left corner of the rectangle, it's width and
%% height will extend to the right and down the screen.
%%
%% This version takes the co-ordinate and size as one big tuple.
%%
%% @spec draw_rect( G::graphics(), {X::float(), Y::float(), Width::float(), Height::float()} ) -> ok
draw_rect( G, {X, Y,   Width, Height} ) -> draw_rect( G, X, Y, Width, Height ).

%%      draw_rect
%%
%% @doc Draws the outline of a rectangle.
%% The location given is the top left corner of the rectangle, it's width and
%% height will extend to the right and down the screen.
%%
%% This takes the location and size and seperate tuples.
%% 
%% @spec draw_rect( G::graphics(), {X::float(), Y::float()}, {Width::float(), Height::float()} ) -> ok
draw_rect( G, {X, Y}, {Width, Height} ) -> draw_rect( G, X, Y, Width, Height ).

%%      draw_rect
%%
%% @doc Draws the outline of a rectangle.
%% The location given is the top left corner of the rectangle, it's width and
%% height will extend to the right and down the screen.
%%
%% This takes the co-ordinates and sizes as seperate values.
%%
%% @spec draw_rect( G::graphics(), X::float(), Y::float(), Width::float(), Height::float() ) -> ok
draw_rect(_G,  X, Y,   Width, Height)  ->
    gl:'begin'(?GL_LINE_LOOP),
    gl:vertex2f(X,       Y),
    gl:vertex2f(X+Width, Y),
    gl:vertex2f(X+Width, Y+Height),
    gl:vertex2f(X,       Y+Height),
    gl:'end'().

%%      fill_rect
%%
%% @doc Draws a filled rectangle.
%% @spec fill_rect( G::graphics(), {X::float(), Y::float(), Width::float(), Height::float()} ) -> ok
fill_rect(G, {X, Y,   Width, Height}) -> fill_rect(G, X, Y, Width, Height).

%%      fill_rect
%%
%% @doc Draws a filled rectangle.
%% @spec fill_rect( G::graphics(), {X::float(), Y::float()}, {Width::float(), Height::float()} ) -> ok
fill_rect(G, {X, Y}, {Width, Height}) -> fill_rect(G, X, Y, Width, Height).

%%      fill_rect
%%
%% @doc Draws a filled rectangle.
%% @spec fill_rect( G::graphics(), X::float(), Y::float(), Width::float(), Height::float() ) -> ok
fill_rect(_G,  X, Y,   Width, Height)  ->
    gl:'begin'(?GL_QUADS),
    gl:vertex2f(X,       Y),
    gl:vertex2f(X+Width, Y),
    gl:vertex2f(X+Width, Y+Height),
    gl:vertex2f(X,       Y+Height),
    gl:'end'().

%%      draw_oval
%%
%% @doc Draws the outline of an oval.
%% @spec draw_oval( G::graphics(), {X::float(), Y::float()}, {Width::float(), Height::float()} ) -> ok
draw_oval(G, {X, Y}, {Width, Height})           -> draw_oval(G, X, Y, Width, Height);

%%      draw_oval
%%
%% @doc Draws the outline of an oval.
%% @spec draw_oval( G::graphics(), {X::float(), Y::float(), Width::float(), Height::float()}, Segments::int() ) -> ok
draw_oval(G, {X, Y,   Width, Height}, Segments) -> draw_oval(G, X, Y, Width, Height, Segments).

%%      draw_oval
%%
%% @doc Draws the outline of an oval.
%% @spec draw_oval( G::graphics(), {X::float(), Y::float()}, {Width::float(), Height::float()}, Segments::int() ) -> ok
draw_oval(G, {X, Y}, {Width, Height}, Segments) -> draw_oval(G, X, Y, Width, Height, Segments).

%%      draw_oval
%%
%% @doc Draws the outline of an oval.
%% @spec draw_oval( G::graphics(), {X::float(), Y::float(), Width::float(), Height::float()} ) -> ok
draw_oval(G, {X, Y,   Width, Height})           -> draw_oval(G, X, Y, Width, Height).

%%      draw_oval
%%
%% @doc Draws the outline of an oval.
%% @spec draw_oval( G::graphics(), X::float(), Y::float(), Width::float(), Height::float() ) -> ok
draw_oval(G,  X, Y,   Width, Height )           -> draw_oval(G, X, Y, Width, Height, ?GRAPHICS_DEFAULT_NUM_SEGMENTS).

%%      draw_oval
%%
%% @doc Draws the outline of an oval.
%%
%% An oval is actually just a polygon, it's not a true cirular shape. The number
%% of segments states how many edges the circle will have, and if you have enough
%% then it looks like a smooth circle.
%%
%% @spec draw_oval( G::graphics(), X::float(), Y::float(), Width::float(), Height::float(), Segments::int() ) -> ok
draw_oval(_G,  X, Y,   Width, Height,  Segments) ->
    HalfWidth  = Width  / 2,
    HalfHeight = Height / 2,
    CenterX    = X + HalfWidth,
    CenterY    = Y + HalfHeight,
    AngleIncrement = util:to_radians(360.0 / Segments),

    gl:'begin'(?GL_LINE_LOOP),
    util:for( Segments,
        fun(I) ->
           SegmentX = CenterX + HalfWidth  * math:cos(AngleIncrement*I),
           SegmentY = CenterY + HalfHeight * math:sin(AngleIncrement*I),
           gl:vertex2d(SegmentX, SegmentY)
        end
    ),
    gl:'end'().

%%      fill_rect
%%
%% @doc Draws a filled oval.
%% @spec fill_oval( G::graphics(), {X::float(), Y::float()}, {Width::float(), Height::float()} ) -> ok
fill_oval(G, {X, Y}, {Width, Height})           -> fill_oval(G, X, Y, Width, Height);

%%      fill_rect
%%
%% @doc Draws a filled oval.
%% @spec fill_oval( G::graphics(), {X::float(), Y::float(), Width::float(), Height::float()} Segments::int() ) -> ok
fill_oval(G, {X, Y,   Width, Height}, Segments) -> fill_oval(G, X, Y, Width, Height, Segments).

%%      fill_rect
%%
%% @doc Draws a filled oval.
%% @spec fill_oval( G::graphics(), {X::float(), Y::float()}, {Width::float(), Height::float()}, Segments::int() ) -> ok
fill_oval(G, {X, Y}, {Width, Height}, Segments) -> fill_oval(G, X, Y, Width, Height, Segments).

%%      fill_rect
%%
%% @doc Draws a filled oval.
%% @spec fill_oval( G::graphics(), {X::float(), Y::float(), Width::float(), Height::float()} ) -> ok
fill_oval(G, {X, Y,   Width, Height})           -> fill_oval(G, X, Y, Width, Height).

%%      fill_rect
%%
%% @doc Draws a filled oval.
%% @spec fill_oval( G::graphics(), X::float(), Y::float(), Width::float(), Height::float() ) -> ok
fill_oval(G,  X, Y,   Width, Height )           -> fill_oval(G, X, Y, Width, Height, ?GRAPHICS_DEFAULT_NUM_SEGMENTS).

%%      fill_rect
%%
%% @doc Draws a filled oval.
%%
%% The x and y location is the top left-corner of the oval and it will stretch
%% to the right and down the screen. The number of segments states how many
%% edges the oval will have. The more, the more detailed it will look (but it
%% takes longer to render).
%%
%% @spec fill_oval( G::graphics(), X::float(), Y::float(), Width::float(), Height::float(), Segments::int() ) -> ok
fill_oval(_G,  X, Y,   Width, Height,  Segments) ->
    HalfWidth  = Width  / 2,
    HalfHeight = Height / 2,
    CenterX    = X + HalfWidth,
    CenterY    = Y + HalfHeight,
    AngleIncrement = util:to_radians(360.0 / Segments),

    gl:'begin'(?GL_TRIANGLE_FAN),
    gl:vertex2d(CenterX, CenterY),
    util:for( Segments,
        fun(I) ->
           SegmentX = CenterX + HalfWidth  * math:cos(AngleIncrement*I),
           SegmentY = CenterY + HalfHeight * math:sin(AngleIncrement*I),
           gl:vertex2d(SegmentX, SegmentY)
        end
    ),
    gl:vertex2d(X + Width, CenterY),
    gl:'end'().

%%      draw_point
%%
%% @doc Draws a single point at the position given.
%% @spec( G::graphics(), {X::float(), Y::float()} ) -> ok
draw_point( G, {X, Y}) ->draw_point( G, X, Y ).

%%      draw_point
%%
%% @doc Draws a single point at the position given.
%% @spec( G::graphics(), X::float(), Y::float() ) -> ok
draw_point(_G,  X, Y )  ->
    gl:'begin'(?GL_POINTS),
    gl:vertex2d(X, Y),
    gl:'end'().


%%      draw_image
%%
%% @doc Draws the given image at the location given.
%% @spec draw_image( G::graphics(), Img::image(), {X::float(), Y::float()} ) -> ok
draw_image(G, Img, {X, Y}) ->
    draw_image(G, Img, {X, Y}, image:get_size(Img)).

%%      draw_image
%%
%% @doc Draws the given image at the location given.
%% @spec draw_image( G::graphics(), Img::image(), {X::float(), Y::float()}, {Width::float(), Height::float()} ) -> ok
draw_image(G, Img, {X, Y}, {Width, Height})  ->
    draw_image(G, Img, {X, Y}, {Width, Height}, false);

%%      draw_image
%%
%% @doc Draws the given image at the location given.
%% @spec draw_image( G::graphics(), Img::image(), {X::float(), Y::float()}, IsCentered::boolean() ) -> ok
draw_image(G, Img, {X, Y}, IsCentered) ->
    draw_image(G, Img, {X, Y}, image:get_size(Img), IsCentered);

%%      draw_image
%%
%% @doc Draws the given image at the location given.
%% @spec draw_image( G::graphics(), Img::image(), X::float(), Y::float() ) -> ok
draw_image(G, Img, X, Y) ->
    draw_image(G, Img, {X, Y}).

%%      draw_image
%%
%% @doc Draws the given image at the location given.
%% @spec draw_image( G::graphics(), Img::image(), {X::float(), Y::float()}, {Width::float(), Height::float()}, IsCentered::boolean() ) -> ok
draw_image(_G, Img, {X, Y}, {Width, Height}, IsCentered) ->
    draw_image_inner(Img, {X, Y}, {Width, Height}, IsCentered);

%%      draw_image
%%
%% @doc Draws the given image at the location given.
%% @spec draw_image( G::graphics(), Img::image(), X::float(), Y::float(), IsCentered::boolean() ) -> ok
draw_image(G, Img, X, Y, IsCentered) ->
    draw_image(G, Img, {X, Y}, IsCentered).

%%      draw_image
%%
%% @doc Draws the given image at the location given.
%% @spec draw_image( G::graphics(), Img::image(), X::float(), Y::float(), Width::float(), Height::float() ) -> ok
draw_image(G, Img, X, Y, Width, Height) ->
    draw_image(G, Img, {X, Y}, {Width, Height}).

%%      draw_image
%%
%% @doc Draws the given image at the location given.
%%
%% The width and height allows the image to be re-scaled to that width and height.
%% The IsCentered states if the image should be drawn centred around the location
%% given, or not. So when IsCentered is true the X and Y is the centre of where
%% the image will be drawn. When IsCentered is false it is the top left-corner.
%%
%% @spec draw_image( G::graphics(), Img::image(), X::float(), Y::float(), Width::float(), Height::float(), IsCentered::boolean() ) -> ok
draw_image(G, Img, X, Y, Width, Height, IsCentered) ->
    draw_image(G, Img, {X, Y}, {Width, Height}, IsCentered).

draw_image_rotated( G, Img, {X, Y}, {Width, Height}, Angle ) ->
    draw_image_rotated( G, Img, X, Y, Width, Height, Angle );
draw_image_rotated( G, Img, X, Y, Angle ) ->
    draw_image_rotated( G, Img, {X, Y}, image:get_size(Img), Angle ).
draw_image_rotated( G, Img, {X, Y}, Angle ) ->
    draw_image_rotated( G, Img, {X, Y}, image:get_size(Img), Angle ).
draw_image_rotated(_G, Img, X, Y, Width, Height, Angle ) ->
    Degrees = util:to_degrees( Angle ),
    gl:translatef( X, Y, 0.0 ),
    gl:rotatef( Degrees, 0.0, 0.0, 1.0 ),
    draw_image_inner( Img, {0, 0}, {Width, Height}, true ),
    gl:rotatef( -Degrees, 0.0, 0.0, 1.0 ),
    gl:translatef( -X, -Y, 0.0 ).

%%      draw_image_inner
%%
%% @doc This is the real, internal image drawing code that does the work.
%% @spec draw_image_inner( Img::image(), {X::float(), Y::float()}, {Width::float(), Height::float()}, IsCentered::boolean() ) -> ok
draw_image_inner(Img, {X, Y}, {Width, Height}, IsCentered) ->
    TexCoordX = Img#img.drawWidth,
    TexCoordY = Img#img.drawHeight,
    
    if
        IsCentered -> DrawX = X - Width/2, DrawY = Y - Height/2;
        true       -> DrawX = X          , DrawY = Y
    end,
    
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, Img#img.glTextureID),
    gl:'begin'(?GL_QUADS),
    
    % bottom left
    gl:texCoord2d(0          , 0.0          ),
    gl:vertex2f(DrawX        , DrawY        ),
    % bottom right
    gl:texCoord2d(TexCoordX  , 0.0          ),
    gl:vertex2f(DrawX+Width  , DrawY        ),
    % top right
    gl:texCoord2d(TexCoordX  , TexCoordY    ),
    gl:vertex2f(DrawX+Width  , DrawY+Height ),
    % top left
    gl:texCoord2d(0          , TexCoordY    ),
    gl:vertex2f(DrawX        , DrawY+Height ),

    gl:'end'(),
    gl:disable(?GL_TEXTURE_2D).

%%      draw_polygon
%%
%% @doc Draws the outline of a closed polygon to the screen using the given list of points.
%% @spec draw_polygon( G::graphics(), Points::[{X::float(), Y::float()}] ) -> ok
draw_polygon(G, Points)           -> draw_polygon(G, Points, {0, 0}).

%%      draw_polygon
%%
%% @doc Draws the outline of a closed polygon to the screen using the given list of points.
%% @spec draw_polygon( G::graphics(), Points::[{X::float(), Y::float()}], X::float(), Y::float() ) -> ok
draw_polygon(G, Points, X, Y)     -> draw_polygon(G, Points, {X, Y}).

%%      draw_polygon
%%
%% @doc Draws the outline of a closed polygon to the screen using the given list of points.
%%
%% The point will be translated by the X and Y given. This allows the points to
%% be a shape around the 0,0 co-ordinate, and then you can translate this to be
%% drawn at X,Y.
%%
%% Points should be a list of X,Y tuples.
%%
%% @spec draw_polygon( G::graphics(), {X::float(), Y::float()}, Points::[{X::float(), Y::float()}] ) -> ok
draw_polygon(_G, {X, Y}, Points) ->
    gl:'begin'(?GL_LINE_LOOP),
    gl_plot_points(X, Y, Points),
    gl:'end'().

%%      fill_polygon
%%
%% @doc Fills a closed polygon to the screen using the points given.
%% @spec fill_polygon( G::graphics(), Points::[{X::float(), Y::float()}] ) -> ok
fill_polygon(G, Points)           -> fill_polygon(G, Points, {0, 0}).

%%      fill_polygon
%%
%% @doc Fills a closed polygon to the screen using the points given.
%% @spec fill_polygon( G::graphics(), Points::[{X::float(), Y::float()}], X::float(), Y::float() ) -> ok
fill_polygon(G, Points, X, Y)     -> fill_polygon(G, Points, {X, Y}).

%%      fill_polygon
%%
%% @doc Fills a closed polygon to the screen using the points given.
%% The location given will translate it to that location before being drawn.
%%
%% @spec fill_polygon( G::graphics(), {X::float(), Y::float()}, Points::[{X::float(), Y::float()}] ) -> ok
fill_polygon(_G, {X, Y}, Points) ->
    gl:'begin'(?GL_POLYGON),
    gl_plot_points(X, Y, Points),
    gl:'end'().

%%      gl_plot_points
%%
%% @doc Iterates through the given list of points plotting them all using gl:vertex.
%% glBegin and glEnd should be called before and after this function. The points
%% will be translated by the given X and Y values.
%%
%% @spec gl_plot_points( X::float(), Y::float(), Points::[] ) -> ok
gl_plot_points(_X, _Y, []) -> ok;
gl_plot_points(X, Y, [{PointX, PointY} | Points]) ->
    gl:vertex2d(X + PointX, Y + PointY),
    gl_plot_points(X, Y, Points).

%%      draw_text
%%
%% @doc Draws the text to the display using the default font.
%% @spec draw_text( G::graphics(), Text::string(), XY::{X::number(), Y::number()} ) -> ok
%% @equiv draw_text( G, Text, X, Y, none )
draw_text( G, Text, {X, Y} )       -> draw_text( G, Text, X, Y ).

%%      draw_text
%%
%% @doc Draws the text to the display using the Font given.
%% @spec draw_text( G::graphics(), Text::string(), {X::number(), Y::number()}, Font::wxFont() | none ) -> ok
%% @equiv draw_text( G, Text, X, Y, Font )
draw_text( G, Text, {X, Y}, Font ) -> draw_text( G, Text, X, Y, Font );

%%      draw_text
%%
%% @doc Draws the text to the display using the default font.
%% @spec draw_text( G::Graphics(), Text::string(), {X::number(), Y::number()} ) -> ok
%% @equiv draw_text( G, Text, X, Y, none ) -> ok
draw_text( G, Text, X, Y )         -> draw_text( G, Text, X, Y, none ).

%%      draw_text
%%
%% @doc Draws the given piece of text to the display.
%% The X and Y co-ordinates correspond to the top left corner of the text when
%% drawn. The Font is an optional wxFont to use when drawing. If ommitted then
%% the default Font is used.
%%
%% @spec draw_text( G::graphics(), Text::string(), X::number(), Y::number(), Font::wxFont() | none ) -> ok
draw_text( G, Text, X, Y, Font ) ->
    DrawImg = new_text_image_inner( Text, Font ),
    draw_image( G, DrawImg, X, Y, false ),
    destroy_image( DrawImg ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Getters and Setters
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      get_color
%%
%% @doc Returns the current colour being used for drawing.
%% Each of the colour components is a floating point value from 0.0 to 1.0.
%% 
%% get_color( G::graphics() ) -> { Red::float(), Green::float(), Blue::float(), Alpha::float() }
get_color( _G ) -> get( ?GRAPHICS_COLOR ).

%%      set_color
%%
%% @doc Sets the color to use when drawing.
%% @spec set_color( G::graphics(), {Red::float(), Green::float(), Blue::float()} ) -> ok
set_color(G, {Red, Green, Blue}) -> set_color( G, Red, Green, Blue );

%%      set_color
%%
%% @doc Sets the color to use when drawing.
%% @spec set_color( G::graphics(), {Red::float(), Green::float(), Blue::float(), Alpha::float()} ) -> ok
set_color(G, {Red, Green, Blue, Alpha}) -> set_color( G, Red, Green, Blue, Alpha ).

%%      set_color
%%
%% @doc Sets the color to use when drawing.
%% The alpha component is left unchanged with this call.
%%
%% @spec set_color( G::graphics(), Red::float(), Green::float(), Blue::float() ) -> ok
set_color(G, Red, Green, Blue) -> set_color(G, Red, Green, Blue, color:get_alpha( get_color(G) )).

%%      set_color
%%
%% @doc Sets the color to use when drawing.
%%
%% All colour components (the Red, Green and Blue) must be a float from 0.0 to
%% 1.0 (inclusive).
%%
%% @spec set_color( G::graphics(), Red::float(), Green::float(), Blue::float(), Alpha::float ) -> ok
set_color(_G, Red, Green, Blue, Alpha) ->
    gl:color4f(Red, Green, Blue, Alpha),
    put( ?GRAPHICS_COLOR, {Red, Green, Blue, Alpha} ).

%%      get_clear_color
%%
%% @doc Returns the colour used for clearing the screen.
%%
%% get_clear_color( G::graphics() ) -> { Red::float(), Green::float(), Blue::float(), Alpha::float() }
get_clear_color( _G ) -> get( ?GRAPHICS_CLEAR_COLOR ).

%%      set_clear_color
%%
%% @doc Sets the clear color for this graphics object.
%% The alpha value will be left unchanged after this call.
%%
%% @spec set_clear_color( G::graphics(), {Red::float(), Green::float(), Blue::float()} ) -> ok
set_clear_color(G, {Red, Green, Blue}) ->
    set_clear_color( G, Red, Green, Blue );

%%      set_clear_color
%%
%% @doc Sets the clear color for this graphics object.
%% @spec set_clear_color( G::graphics(), {Red::float(), Green::float(), Blue::float(), Alpha::float()} ) -> ok
set_clear_color(G, {Red, Green, Blue, Alpha}) ->
    set_clear_color( G, Red, Green, Blue, Alpha ).

%%      set_clear_color
%%
%% @doc Sets the clear color for this graphics object.
%% The alpha value will be left unchanged after this call.
%%
%% @spec set_clear_color( G::graphics(), Red::float(), Green::float(), Blue::float() ) -> ok
set_clear_color(G, Red, Green, Blue) ->
    set_clear_color( G, Red, Green, Blue, color:get_alpha( get_color(G) ) ).

%%      set_clear_color
%%
%% @doc Sets the clear color for this graphics object.
%% This is the color that will be used when the Graphics object is cleared.
%%
%% All colour components (the Red, Green, Blue and alpha) must be a float from
%% 0.0 to 1.0 (inclusive).
%%
%% Note that this is for the clear command, so as long as that is unsupported
%% this method is pointless.
%%
%% @spec set_clear_color( G::graphics(), Red::float(), Green::float(), Blue::float(), Alpha::float ) -> ok
set_clear_color( _G, Red, Green, Blue, Alpha ) ->
    gl:clearColor( Red, Green, Blue, Alpha ),
    put( ?GRAPHICS_CLEAR_COLOR, {Red, Green, Blue, Alpha} ).

%%      get_size
%%
%% @doc Returns the size of the drawing area in the window.
%% This is not the size of the whole window.
%%
%% @spec get_size( G::graphics() ) -> { Width::int(), Height::int() }
get_size(G) -> G#glContext.size.

%%      get_width
%%
%% @doc Returns the width of the drawing area inside of the window.
%% @spec get_width( G::graphics() ) -> Width::float()
get_width(G) ->
    {Width, _Height} = get_size(G),
    Width.

%%      get_height
%%
%% @doc Returns the height of the drawing area inside of the window.
%% @spec get_height( G::graphics() ) -> Height::float()
get_height(G) ->
    {_Width, Height} = get_size(G),
    Height.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Utility
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      new_image_inner
%%
%% @doc This code handles loading an image from the given filename and then turning it into a texture.
%% @spec new_image_inner( FilerName::string() ) -> Img::image()
new_image_inner(FileName) ->
    WXImage = wxImage:new( FileName ),
    Img     = create_image( WXImage ),
    wxImage:destroy( WXImage ),
    Img.

%%      new_text_image_inner
%%
%% @doc Generates a new texture with the text given rendered to it using the given font.
%% Note that the image will be cropped to as small as possible. There will be no
%% border around the Text.
%%
%% @spec new_text_image_inner( Text::string(), Font::wxFont() | none ) -> TextImage::image()
new_text_image_inner(Text, Font) ->
    SizeBitmap = wxBitmap:new( 0, 0 ),
    DC = wxMemoryDC:new(),

    wxMemoryDC:selectObject( DC, SizeBitmap ),
    if
        (Font /= none) ->
            wxDC:setFont( DC, Font );
        true ->
            ok
    end,
    {Width, Height} = wxDC:getTextExtent( DC, Text ),

    Bitmap = wxBitmap:new( Width, Height, [{depth, 24}] ),

    wxMemoryDC:selectObject( DC, Bitmap ),
    wxDC:setTextForeground( DC, {255, 255, 255} ),
    wxDC:setBackgroundMode( DC, ?wxTRANSPARENT ),
    wxDC:drawText( DC, Text, {0, 0} ),

    Img = wxBitmap:convertToImage( Bitmap ),
    DrawImg = create_image( Img, 0 ), % 0 is black in RGB

    % Destroy Everything!
    wxBitmap:destroy( Bitmap ),
    wxImage:destroy( Img ),
    wxMemoryDC:destroy( DC ),

    DrawImg.

%%      create_image
%%
%% @doc Loads the given wx image as a texture and returns an image representing this.
%% If the image has alpha transparency, then it will be loaded as a texture with
%% red, green, blue and alpha components. Otherwise the alpha will be ignored
%% and there will be no transparency.
%%
%% @spec create_image( WXImage::wx_image() ) -> Image::image()
create_image( WXImage ) -> create_image( WXImage, no_mask ).

%%      create_image
%%
%% @doc Loads the given wx image as a texture and returns an image representation.
%% If the image has alpha transparency, then this is used for it's alpha
%% component. If it does not and a MaskRGB colour has been supplied, then all
%% pixels that match the MaskRGB will have their alpha set to be fully
%% transparent. Otherwise the image will be loaded as an RGB image.
%%
%% Note that masking doesn't work if the image has an alpha channel.
%%
%% @spec create_image( WXImage::wxImage(), MaskRGB::integer() ) -> NewImg::image()
create_image( WXImage, MaskRGB ) ->
    Width       = wxImage:getWidth  ( WXImage  ),
    TexWidth    = get_power_of_two  ( Width    ),
    Height      = wxImage:getHeight ( WXImage  ),
    TexHeight   = get_power_of_two  ( Height   ),

    [GLTextureID] = gl:genTextures( 1 ),
    gl:bindTexture  ( ?GL_TEXTURE_2D, GLTextureID ),
    gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR ),
    gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR ),

    HasAlpha = wxImage:hasAlpha( WXImage ),
    if
        HasAlpha ->
            PixelData = image_mix_data(
                    wxImage:getData(  WXImage ),
                    wxImage:getAlpha( WXImage ) ),
            GLNumBytes = ?GL_RGBA8,
            GLTexMode  = ?GL_RGBA,
            PixelBits  = 4;
        is_number( MaskRGB ) ->
            PixelData = image_mask_to_rgba( MaskRGB, wxImage:getData( WXImage ) ),
            GLNumBytes = ?GL_RGBA8,
            GLTexMode  = ?GL_RGBA,
            PixelBits  = 4;
        true ->
            PixelData = wxImage:getData( WXImage ),
            GLNumBytes = ?GL_RGB8,
            GLTexMode  = ?GL_RGB,
            PixelBits  = 3
    end,
    
    if
        (not (Width == TexWidth)) or (not (Height == TexHeight)) ->
            GLData = expand_data( PixelBits, PixelData, Width, Height, TexWidth, TexHeight );
        true ->
            GLData = PixelData
    end,
    
    gl:texImage2D( ?GL_TEXTURE_2D, 0, GLNumBytes, TexWidth, TexHeight, 0, GLTexMode, ?GL_UNSIGNED_BYTE, GLData    ),
    
    #img{
        width       = Width,
        height      = Height,
        drawWidth   = Width  / TexWidth,
        drawHeight  = Height / TexHeight,
        glTextureID = GLTextureID,
        destroyer   = self()
    }.

%%          expand_data
%%
%% @doc This function, and those below it, deal with re-formatting the given
%% array of pixel data so that it increases size from the given Width and
%% Height, to the given TexWidth and TexHeight. The data is stored in a 1
%% dimensional array of pixels, so two functions are applied to it in order to
%% increase it's size.
%%
%% First pad_data pads out the rows so that they are wider. Then add_row_data
%% adds a block of data to the end to make it taller.
%%
%% The given PixelBits refers to the number of bytes used to represent each
%% pixel in the PixelData.
expand_data(PixelBits, PixelData, Width, Height, TexWidth, TexHeight) ->
    add_row_data(
            TexWidth*PixelBits, TexHeight-Height,
            pad_data(
                    Width*PixelBits, (TexWidth-Width)*PixelBits,
                    PixelData, <<>>,
                    Height, Width*PixelBits )
    ).

%%          pad_data
%%
%% @doc Pads each row of the given data in turn by the given amount of padding.
%% The Width and WPadding are both measured in bytes.
pad_data(_Width, _WPadding, _OldPixels, NewPixels, 0, _W) ->
    NewPixels;
pad_data(Width, WPadding, OldPixels, <<NewPixels/binary>>, H, 0) ->
    pad_data(
            Width, WPadding,
            OldPixels, <<NewPixels/binary, 0:(WPadding*8)>>,
            H-1, Width );
pad_data(Width, WPadding, <<Bit:8, OldPixels/binary>>, <<NewPixels/binary>>, H, W) ->
    pad_data(
            Width, WPadding,
            OldPixels, <<NewPixels/binary, Bit:8>>,
            H  , W-1   ).

%%          add_row_data
%%
%% @doc Adds a big chunk of data to the given pixels and returns the result.
add_row_data( W, H, <<Pixels/binary>> ) -> <<Pixels/binary, 0:(W*H*8)>>.

%%      image_mix_data
%%
%% @doc Mixes the two binary streams together into one stream.
%% The first stream is a series of 8-bit unsigned values representing the red,
%% green and blue of each pixel in order. This constantly repeats (i.e. R,G,B,
%% R,G,B, R,G,B ... )
%%
%% The second stream contains the alpha component for these pixels, again it's
%% stored as 8-bit unsigned values.
%%
%% The result is a stream containing these values combined as repeating
%% R, G, B, A components. (i.e. R,G,B,A, R,G,B,A, R,G,B,A, ... ).
image_mix_data( RGBData, AlphaData ) ->
    image_mix_data( RGBData, AlphaData, <<>> ).
image_mix_data( <<>>, <<>>, NewData ) ->
    NewData;
image_mix_data( <<RGB:24, RGBData/binary>>,
                <<A:8, AlphaData/binary>>,
                <<NewData/binary>> ) ->
    image_mix_data( RGBData, AlphaData, <<NewData/binary, RGB:24, A:8>> ).

%%      image_mask_to_rgba
%%
%% @doc For converting an image from the RGB format to RGBA, where the mask colour is no transparent.
%% Given a binary of RGB pixels, this returns a binary of RGBA pixel. But
%% whenver an RGB value matches the masking colour, the alpha will be set to 0.
%% Otherwise it is set to 255.
%%
%% The MaskRGB is a 24-bit RGB colour value.
image_mask_to_rgba(  MaskRGB, RGBData )       -> image_mask_to_rgba( MaskRGB, RGBData, <<>> ).
image_mask_to_rgba( _MaskRGB, <<>>, NewData ) -> NewData;
image_mask_to_rgba(  MaskRGB, <<RGB:24, RGBData/binary>>, <<NewData/binary>> ) ->
    if
        RGB =:= MaskRGB -> image_mask_to_rgba( MaskRGB, RGBData, <<NewData/binary, RGB:24,   0:8>> );
        true            -> image_mask_to_rgba( MaskRGB, RGBData, <<NewData/binary, RGB:24, 255:8>> )
    end.

%%      get_power_of_two
%%
%% @doc If the given Length is already a power of two number, then it is returned.
%% Otherwise the next power of two number which is larger is returned.
%%
%% @spec get_power_of_two( Length::int() ) -> NewLength::int()
get_power_of_two(Length) ->
    if
        (Length bor (Length-1) == 0) -> Length;
        true -> first_greater_power_of_two(Length)
    end.

%%      first_greater_power_of_two
%%
%% @doc Finds and then returns the first number that is a power of two and is greater then the given value.
%% @spec first_greater_power_of_two(Length::int()) -> NewLength::int()
first_greater_power_of_two(Length) -> first_greater_power_of_two(Length, 2).

%%      first_greater_power_of_two
%%
%% @doc If the given power of two is greater then length, then it is returned.
%% Otherwise this will call itself with double the power of two value.
%% @spec first_greater_power_of_two(Length::int(), PowerOfTwo::int()) -> NewLength::int()
first_greater_power_of_two(Length, PowerOfTwo) ->
    if
        (PowerOfTwo >= Length) -> PowerOfTwo;
        true -> first_greater_power_of_two(Length, PowerOfTwo*2)
    end.

%%      destroy_image
%%
%% @doc Unloads the given image from the graphics card.
%% Continuing to use this image will be unsafe and cause unknown issues.
%%
%% @spec destroy_image( Image::image() ) -> ok
destroy_image( Image ) -> gl:deleteTextures([ Image#img.glTextureID ]).
