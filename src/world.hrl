

-record(worldInternal, {actors, nextActors, parent}).

%% this is the structure of the record sent to the Worlds
%% Actor, the WorldActor.
-record(worldAct, { data, actors, senderPID, parent }).

-define( WORLD_ADD_ACTOR_MESSAGE,            world_add_new_actor         ).
-define( WORLD_REMOVE_ACTOR_MESSAGE,         world_remove_actor          ).
-define( WORLD_GET_ACTOR_MESSAGE,            world_get_new_actor         ).
-define( WORLD_SET_PAINT_ORDER_MESSAGE,      world_set_paint_order_actor ).
-define( WORLD_REMOVE_ALL_ACTORS_MESSAGE,    world_remove_all_actors     ).

-define( WORLD_ADD_ACTORS_MESSAGE,           world_add_new_actors        ).
-define( WORLD_REMOVE_ACTORS_MESSAGE,        world_remove_actors         ).

-define( WORLD_APPLY_TO_INTERSECTING_ACTORS_MESSAGE, world_apply_to_intersecting_actors_message ).
-define( WORLD_APPLY_TO_INTERSECTING_ACTORS_MESSAGE_NO_NAME, world_apply_to_intersecting_actors_no_name_message ).
-define( WORLD_APPLY_TO_INTERSECTING_ACTOR_MESSAGE , world_apply_to_intersecting_actor_message  ).
-define( WORLD_APPLY_TO_INTERSECTING_ACTOR_MESSAGE_NO_NAME, world_apply_to_intersecting_actor_no_name_message  ).
