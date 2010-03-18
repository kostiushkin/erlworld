
%%%             controls_handler
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This is a data collection for storing the current state of the controls. You
%%% can give it mouse locations and the press and release of mouse buttons and
%%% keys. You can also grab the current state of the controls using the
%%% get_controls function.
%%% 

-module( controls_handler ).
-author("Joseph Lenton").

-include("controls_handler.hrl").

-export([
    new/0
]).

-export([
    get_controls/1
]).

-export([
    set_mouse_location/2,
    set_mouse_down/2, set_mouse_up/2,
    set_key_char/2, set_key_down/2, set_key_up/2
]).

% this represents no character or key_down/key_up code
-define( NO_KEY, 0 ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      new
%% 
%% @doc Creates a new process that stores controls changes.
%% This can then be pinged for a snapshot of the controls.
%% 
%% @spec new() -> ControlsHandler::controls_handler()
new() ->
    StartState = {
            new_control_state(),
            {
                    ?NO_KEY,    % the last KeyDown scancode
                    ?NO_KEY,    % the last Char unicode character
                    dict:new()  % a map from KeyDowns to Chars (so we can remove chars on KeyUp)
            }
    },
    function_server:new( StartState,
            [ %Message Functions
                    { ?CONTROLS_HANDLER_SET_MOUSE_LOCATION,
                            fun(XY, {State, NextKey}) ->
                                { State#control_state{ mouse_position = XY }, NextKey }
                            end },
                    { ?CONTROLS_HANDLER_SET_MOUSE_DOWN,
                            fun(Button, {State, NextKey}) ->
                                MButtons = State#control_state.mouse_down,
                                { State#control_state{ mouse_down = dict:store(Button, true, MButtons) }, NextKey }
                            end },
                    { ?CONTROLS_HANDLER_SET_KEY_DOWN,
                            fun(Key, {State, {_KeyDown, Char, DownKeys}}) ->
                                set_key_down_inner(State, Key, Char, DownKeys)
                            end },
                    { ?CONTROLS_HANDLER_SET_KEY_CHAR,
                            fun(Char, {State, {KeyDown, _OldChar, DownKeys}}) ->
                                set_key_down_inner(State, KeyDown, Char, DownKeys)
                            end },
                    { ?CONTROLS_HANDLER_SET_MOUSE_UP,
                            fun(Button, {State, NextKey}) ->
                                MButtons = State#control_state.mouse_down,
                                { State#control_state{ mouse_down = dict:erase(Button, MButtons) }, NextKey }
                            end },
                    { ?CONTROLS_HANDLER_SET_KEY_UP,
                            fun(Key, {State, NextKey}) ->
                                set_key_up_inner(State, Key, NextKey)
                            end }
            ],
            [ % Request Functions
                    { ?CONTROLS_HANDLER_GET_CONTROLS,
                            fun( _Params, {State, NextKey} ) ->
                                { {State, NextKey}, State }
                            end }
            ]).

%%      newControlState
%%
%% @doc This create a blank controls record.
%% @spec new_control_state() -> Controls::controls_state()
new_control_state() ->
    #control_state{
        mouse_position  = { 0, 0 },
        mouse_down      = dict:new(),
        keys_down       = dict:new()
    }.

%%      set_key_down_inner
%%
%% @doc This this is the code performed in the same process that sets the key to be down.
%% It works the for the key char given. The key and char given are to state
%% which key on the keyboard is literally pressed, and the character
%% representation of that key. For example the A key might be pressed, but the
%% char would be a or A, or any other character mapped to that key.
%%
%% If the given key or char is the ?NO_KEY value, then this will just do nothing.
%% Only if both are present then the character is mapped. The reason behind this
%% is that you are expected to have character and key pressed come in one after
%% another but seperately. So in all cases you just pump what you have through
%% this function and it will setup the state to either be ready for the next
%% char/key press or to set the mapping for the char to be down.
%% 
%% @spec set_key_down_inner( State::controls(), Key::integer(), Char::integer(), DownKeys::dict() ) -> NewState::controls()
set_key_down_inner( State, ?NO_KEY, Char   , DownKeys ) -> { State, {?NO_KEY, Char, DownKeys} };
set_key_down_inner( State, Key    , ?NO_KEY, DownKeys ) -> { State, {Key, ?NO_KEY , DownKeys} };
set_key_down_inner( State, Key    , Char   , DownKeys ) ->
    Keys = State#control_state.keys_down,
    {
        State#control_state{ keys_down = dict:store(Char, true, Keys) },
        { ?NO_KEY, ?NO_KEY, dict:store(Key, Char, DownKeys) }
    }.

%%      set_key_up_inner
%%
%% @doc Very similar to set_key_down_inner, only the key is left up after this press.
%%
%% @spec set_key_up_inner( State::controls(), Key::integer(), {OldKey::integer(), OldChar::integer(), DownKeys::dict()} ) -> NewState::controls()
set_key_up_inner( State, Key, {OldKey, OldChar, DownKeys} ) ->
    case dict:find( Key, DownKeys ) of
        {ok, Char} ->
                Keys = State#control_state.keys_down,
                {
                    State#control_state{ keys_down = dict:erase(Char, Keys) },
                    { OldKey, OldChar, dict:erase(Key, DownKeys) }
                };
        error ->
                { State, {OldKey, OldChar, DownKeys} }
    end.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      set_mouse_location
%%
%% @doc Sets the mouse location stored in this ControlsHandler to the location given.
%%
%% @spec set_mouse_location( CHandler::controls_handler(), Location::{X::interger(), Y::integer} ) -> NewCHandler::controls_handler()
set_mouse_location(CHandler, Location) -> data_server:send(CHandler, ?CONTROLS_HANDLER_SET_MOUSE_LOCATION, Location).

%%      set_mouse_down
%%
%% @doc Sets the button to be down for the value given.
%% The value given should be one of the mouse button constants.
%%
%% @spec set_mouse_down( CHandler::controls_handler(), left | middle | right ) -> NewCHandler::controls_handler()
set_mouse_down(CHandler, Mouse) -> data_server:send(CHandler, ?CONTROLS_HANDLER_SET_MOUSE_DOWN, Mouse).

%%      set_mouse_up
%%
%% @doc Sets the button to be up for the mouse button given.
%% @spec set_mouse_up( CHandler::controls_handler(), left | middle | right ) -> NewCHandler::controls_handler()
set_mouse_up(CHandler, Mouse)   -> data_server:send(CHandler, ?CONTROLS_HANDLER_SET_MOUSE_UP, Mouse).

%%      set_key_char
%%
%% @doc Sets the current key character we are currently looking at.
%% Any keypresses that come in before or after this character which are not
%% mapped will be presumed to map against this char.
%%
%% @spec set_key_char( CHandler::controls_handler(), Key::integer() ) -> NewCHandler::controls_handler()
set_key_char(CHandler, Key)     -> data_server:send(CHandler, ?CONTROLS_HANDLER_SET_KEY_CHAR, Key).

%%      set_key_down
%%
%% @doc Sets the key to be down for the given key code.
%% @spec set_key_down( CHandler::controls_handler(), Key::integer() ) -> NewCHandler::controls_handler()
set_key_down(CHandler, Key)     -> data_server:send(CHandler, ?CONTROLS_HANDLER_SET_KEY_DOWN, Key).

%%      set_key_up
%%
%% @doc Sets the key to be up for the given key code.
%% Any characters mapped against the given key will no longer be mapped as being down.
%%
%% @spec set_key_up( CHandler::controls_handler(), Key::integer() ) -> NewCHandler::controls_handler()
set_key_up(CHandler, Key)       -> data_server:send(CHandler, ?CONTROLS_HANDLER_SET_KEY_UP, Key).

%%      get_controls
%%
%% @doc Requests a snapshot current state of the controls from this Controls Handler.
%% @spec get_controls(CHandler::controls_handler()) -> Controls::controls()
get_controls(CHandler) -> data_server:request(CHandler, ?CONTROLS_HANDLER_GET_CONTROLS).
