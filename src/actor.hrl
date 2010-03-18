%%% 
%%%             Actor
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This defines the messages and data structures used by the
%%% Actor module. It is primarily for internal use and users
%%% of the framework are not expected to import this header or
%%% to know about it's definitions and workings.'

% This is the internal actor structure within it's server. It's the data
% structure for the framework.
-record( actor_server_state, {
    act,        % The Act function for this actor
    paint,      % The Paint function for this actor
    timestamp,  % a unique timestamp given to the actor at the point of creation
    state,      % the actors gameplay state and details
    parent      % the actors current parent, set at the end of each act function
}).

%% These are the messages that are sent between functions the act module.
-define( ACTOR_MESSAGE_ACT                 , actor_act_message                  ).
-define( ACTOR_MESSAGE_ACT_RESPONSE        , actor_message_act_response         ).
-define( ACTOR_MESSAGE_POST_ACT            , actor_message_post_act             ).

-define( ACTOR_MESSAGE_GET_PAINT_AND_STATE , actor_get_paint_and_state_message  ).

-define( ACTOR_MESSAGE_GET_NAME            , actor_get_name_message             ).
-define( ACTOR_MESSAGE_GET_STATE           , actor_get_state_message            ).

-define( ACTOR_MESSAGE_GET_PAINT_FUNC      , actor_get_paint_func_message       ).

-define( ACTOR_MESSAGE_GET_ACT_FUNC        , actor_get_act_func_message         ).

-define( ACTOR_MESSAGE_GET_TIMESTAMP       , actor_get_timestamp_message        ).

-define( ACTOR_MESSAGE_RUN_ON_REMOVE       , actor_run_on_remove                ).

-define( ACTOR_ON_END_OF_MAINLOOP          , actor_on_end_of_mainloop           ).