
-define( MOUSE_LEFT         , left   ).
-define( MOUSE_RIGHT        , right  ).
-define( MOUSE_MIDDLE       , middle ).

-define( CONTROLS_HANDLER_SET_MOUSE_LOCATION    , controls_handler_set_mouse_location   ).
-define( CONTROLS_HANDLER_SET_MOUSE_DOWN        , controls_handler_set_mouse_down       ).
-define( CONTROLS_HANDLER_SET_MOUSE_UP          , controls_handler_set_mouse_up         ).

-define( CONTROLS_HANDLER_SET_KEY_CHAR          , controls_handler_set_key_char         ).
-define( CONTROLS_HANDLER_SET_KEY_DOWN          , controls_handler_set_key_down         ).
-define( CONTROLS_HANDLER_SET_KEY_UP            , controls_handler_set_key_up           ).

% These are used for passing on get and response control signals between actors and mainloops.
-define( CONTROLS_HANDLER_GET_CONTROLS          , controls_handler_get_controls          ).
-define( CONTROLS_HANDLER_GET_CONTROLS_RESPONSE , controls_handler_get_controls_response ).

% this is the record which the controls are stored in.
-record( control_state, {
    mouse_position,
    mouse_down,
    keys_down
}).