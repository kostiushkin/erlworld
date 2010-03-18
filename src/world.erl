
%%%
%%%             world
%%%
%%% @author Joseph Lenton
%%% @doc
%%% The World module is conceptually a sub-class of Actor. Objects created using
%%% this module can always be used in the functions of the Actor module, and
%%% anywhere an Actor can be given.
%%%
%%% This is an Actor that can hold Actors. In order to be a world this uses a
%%% world_state instead of an actor_state.
%%%
%%% In all other ways it runs just like an Actor. It has a state, an act
%%% callback function and a paint callback function. By default the act and
%%% paint callback functions for a World just call to act and paint all of the
%%% Actors the World is holding.
%%% 

-module(world).
-author("Joseph Lenton").

-export([ new/0, new/1, new/2, new/3 ]).
-export([
        add_actor/2, add_actors/2,
        set_paint_order/3,
        remove_actor/2, remove_actors/2,
        remove_all_actors/1
]).
-export([
        is_intersecting_actor/2, is_intersecting_actor/3,
        apply_to_intersecting_actors/3, apply_to_intersecting_actors/4, apply_to_intersecting_actors/5,
        apply_to_intersecting_actor/3 , apply_to_intersecting_actor/4 , apply_to_intersecting_actor/5
]).

-export([
    act/2,
    paint/2,
    send_act/2,
    receive_act/0,

    get_act/1,
    get_paint/1,
    get_state/1,
    get_name/1,
    get_controls/1,

    compare/2
]).

-include("actor.hrl").
-include("world.hrl").
-include("world_state.hrl").
-include("collisions.hrl").

-define( NO_DATA, ok ).
-define( NO_PARENT, no_parent ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Constructors
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%              new
%%
%% @doc Creates a new blank world with an empty state.
%% @spec new() -> World::world()
new()           -> new( world_state:new() ).

%%              new
%%
%% @doc Creates a new world that will update the world_state given.
%% @spec new( State::world_state() ) -> World::world()
new(State)      -> new( new_world_act(), new_world_paint(), State ).

%%              new
%%
%% @doc Creates a blank World which will paint and update with the functions given.
%% @spec new( Act::ActFunc, Paint::PaintFunc ) -> NewWorld::world()
%%          ActFunc   = ( State::world_state(), Parent::pid() ) -> world_state()
%%          PaintFunc = ( State::world_state(), G::graphics() ) -> term()
new(Act, Paint) -> new( Act, Paint, world_state:new() ).

%%              new
%%
%% @doc Creates a new World with that will run the act and paint callbacks given, on the state given.
%% @spec new( Act::ActFunc, Paint::PaintFunc, StartState::world_state() ) -> NewWorld::world()
%%          ActFunc   = ( State::world_state(), Parent::pid() ) -> world_state()
%%          PaintFunc = ( State::world_state(), G::graphics() ) -> term()
new(Act, Paint, StartState) ->
    PID = actor:new( Act, Paint, StartState ),
    function_server:set_message_funcs( PID, [
            { ?ACTOR_MESSAGE_POST_ACT,
                    fun( WorldState, State ) ->
                            InnerState = State#actor_server_state.state,
                            NewWS = world_state:set( WorldState,
                                                    ?WORLD_STATE_ADD_REMOVE_ACTORS,
                                                    world_state:get(InnerState, ?WORLD_STATE_ADD_REMOVE_ACTORS) ),
                            State#actor_server_state{ state = NewWS }
                    end },
            {?ACTOR_ON_END_OF_MAINLOOP,
                    fun( _, State ) ->
                        WS = State#actor_server_state.state,
                        NewWS = world_state:set(
                                add_remove_actors( WS, lists:reverse(world_state:get(WS, ?WORLD_STATE_ADD_REMOVE_ACTORS)) ),
                                ?WORLD_STATE_ADD_REMOVE_ACTORS,
                                []
                        ),
                        world_state:map_actors( NewWS,
                                fun( Actor ) ->
                                    data_server:send( Actor, ?ACTOR_ON_END_OF_MAINLOOP )
                                end ),
                        State#actor_server_state{state = NewWS}
                    end },
            {?WORLD_APPLY_TO_INTERSECTING_ACTORS_MESSAGE ,
                    fun( {Sender, Actor, OtherName, CheckFun, OnCollFunc}, State ) ->
                        WState = State#actor_server_state.state,
                        world_state:apply_to_intersecting_actors( WState, Sender, Actor, OtherName, CheckFun, OnCollFunc ),
                        State
                    end },
            {?WORLD_APPLY_TO_INTERSECTING_ACTORS_MESSAGE_NO_NAME ,
                    fun( {Sender, Actor, CheckFun, OnCollFunc}, State ) ->
                        WState = State#actor_server_state.state,
                        world_state:apply_to_intersecting_actors( WState, Sender, Actor, CheckFun, OnCollFunc ),
                        State
                    end },
            {?WORLD_APPLY_TO_INTERSECTING_ACTOR_MESSAGE ,
                    fun( {Sender, Actor, OtherName, CheckFun, OnCollFunc}, State ) ->
                        WState = State#actor_server_state.state,
                        world_state:apply_to_intersecting_actor( WState, Sender, Actor, OtherName, CheckFun, OnCollFunc ),
                        State
                    end },
            {?WORLD_APPLY_TO_INTERSECTING_ACTOR_MESSAGE_NO_NAME ,
                    fun( {Sender, Actor, CheckFun, OnCollFunc}, State ) ->
                        WState = State#actor_server_state.state,
                        world_state:apply_to_intersecting_actor( WState, Sender, Actor, CheckFun, OnCollFunc ),
                        State
                    end },
            {?WORLD_ADD_ACTOR_MESSAGE ,
                    fun( Actor, State ) ->
                        push_add_remove( State, Actor, add )
                    end },
            {?WORLD_ADD_ACTORS_MESSAGE ,
                    fun( Actors, State ) ->
                        push_add_remove( State, Actors, adds )
                    end },
            {?WORLD_SET_PAINT_ORDER_MESSAGE ,
                    fun( Params, State ) ->
                        push_add_remove( State, Params, set_paint_order )
                    end }
    ]),
    function_server:set_request_funcs( PID, [
            {?WORLD_REMOVE_ACTOR_MESSAGE ,
                    fun( Actor, State ) ->
                        { push_add_remove( State, Actor, remove ), ok }
                    end },
            {?WORLD_REMOVE_ACTORS_MESSAGE ,
                    fun( Actors, State ) ->
                        { push_add_remove( State, Actors, removes ), ok }
                    end },
            {?WORLD_REMOVE_ALL_ACTORS_MESSAGE ,
                    fun( _, State ) ->
                        { push_add_remove( State, remove_all ), ok }
                    end }
    ]),
    PID.

%%              new_world_act
%% 
%% @doc Returns the default act callback to use for updating the actors in this world.
%% @spec new_world_act() -> Act::( State::world_state(), Parent::pid() ) -> world_state()
new_world_act() ->
    fun( State, _Parent ) ->
        world_state:act_actors( State )
    end.

%%              new_world_paint
%%
%% @doc Returns the default paint callback to use for painting the actors in this world.
%% @spec new_world_paint() -> Paint::( State::world_state(), Parent::pid() ) -> term()
new_world_paint() ->
    fun( State, G ) ->
        world_state:paint_actors( State, G )
    end.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%% The add and remove functions performed by the world_state are actually
%%% cached over each frame. This code deals with storing those functions and
%%% then later running each of them in turn.
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          push_add_remove
%%
%% @doc Pushes and operation to perform onto the given server state.
%% An updated server state is then returned.
%%
%% The type describes the operation being performed; e.g. add, remove or
%% set_paint_order.
push_add_remove( State, Params, Type ) ->
    push_add_remove( State, {Params, Type} ).

%%          push_add_remove
%%
%% @doc The same as the other push_add_remove function, only with the values passed as a tuple.
push_add_remove( State, ToPush ) ->
    WState = State#actor_server_state.state,
    NewState = push_add_remove_inner( WState, ToPush ),
    State#actor_server_state { state = NewState }.

push_add_remove_inner( WState, Push ) ->
    world_state:set(
            WState,
            ?WORLD_STATE_ADD_REMOVE_ACTORS,
            [ Push | world_state:get(WState, ?WORLD_STATE_ADD_REMOVE_ACTORS) ]
    ).

add_remove_actors(WState, []) -> WState;
add_remove_actors(WState, [{Actor, add}     | Actors]) -> add_remove_actors( world_state:add_actor(WState, Actor), Actors );
add_remove_actors(WState, [{Actor, remove}  | Actors]) -> add_remove_actors( world_state:remove_actor(WState, Actor), Actors );
add_remove_actors(WState, [{{A, PaintNum}, set_paint_order} | Actors]) ->
    add_remove_actors( world_state:set_paint_order(WState, A, PaintNum), Actors );
add_remove_actors(WState, [{Actor, adds}    | Actors]) -> add_remove_actors( world_state:add_actors(WState, Actor), Actors );
add_remove_actors(WState, [{Actor, removes} | Actors]) -> add_remove_actors( world_state:remove_actors(WState, Actor), Actors );
add_remove_actors(WState, [ remove_all      | Actors]) -> add_remove_actors( world_state:remove_all_actors(WState), Actors ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          add_actor
%%
%% @doc Adds the given Actor to this World.
%% @spec add_actor( World::world(), Actor::actor() ) -> ok
add_actor( World, Actor )    -> data_server:send( World, ?WORLD_ADD_ACTOR_MESSAGE, Actor ).

%%          add_actors
%%
%% @doc Adds a the given list of Actors to this World.
%% @spec add_actors( World::world(), Actors::[actor()] ) -> ok
add_actors( World, Actor )    -> data_server:send( World, ?WORLD_ADD_ACTORS_MESSAGE, Actor ).

%%          remove_actor
%%
%% @doc Removes the given Actor from this World, if it is contained in the World.
%% @spec remove_actor( World::world(), Actor::actor() ) -> ok
remove_actor( World, Actor ) -> data_server:request( World, ?WORLD_REMOVE_ACTOR_MESSAGE, Actor ).

%%          remove_actors
%%
%% @doc Removes the given list of Actors from this World.
%% @spec remove_actors( World::world(), Actors::[actor()] ) -> ok
remove_actors( World, Actor )    -> data_server:request( World, ?WORLD_REMOVE_ACTORS_MESSAGE, Actor ).

%%          remove_all_actors
%%
%% @doc Empties the given World of all of the Actors it is holding.
%% @spec remove_all_actors( World::world() ) -> ok
remove_all_actors( World )   -> data_server:request( World, ?WORLD_REMOVE_ALL_ACTORS_MESSAGE ).

%%          set_paint_order
%%
%% @doc Sets the painting order of the given Actor within this World.
%% @spec set_paint_order( World::world(), Actor::actor(), PaintOrder::integer() ) -> ok
set_paint_order( World, Actor, PaintOrder ) -> data_server:send( World, ?WORLD_SET_PAINT_ORDER_MESSAGE, {Actor, PaintOrder} ).

%%          is_intersecting_actor
%% 
%% @doc This is a very basic check for any intersecting actors.
%% If any actor intersects the actor given then this return true, otherwise
%% false. This ignores the Actor given.
%% 
%% @spec is_intersecting_actor( World::world(), Actor::actor() ) -> IsIntersection::boolean()
is_intersecting_actor( World, Actor ) ->
    apply_to_intersecting_actor( World, Actor, fun(_A) -> ok end ).

%%          is_intersecting_actor
%%
%% @doc This is for testing for an intersection only against actors of a given name.
%% The name given is compared to the name stored in the actor_state of the actors
%% being tested against.
%%
%% @spec is_intersecting_actor( World::world(), Actor::actor(), OtherActorName::atom() ) -> IsIntersection::boolean()
is_intersecting_actor( World, Actor, OtherActorName ) when is_atom( OtherActorName ) ->
    apply_to_intersecting_actor( World, Actor, OtherActorName, fun(_A) -> ok end );

%%          is_intersecting_actor
%%
%% @doc This is a check for any intersecting actors, also based on the function given.
%% If any actor intersects the actor given and passes the function given, then
%% this return true. Otherwise false. This ignores the Actor given.
%%
%% @spec is_intersecting_actor( World::world(), Actor::actor(), CustomCheckFun ) -> IsIntersection::boolean()
%%      CustomCheckFun = ( OtherActorState::actor_state() ) -> boolean()
is_intersecting_actor( World, Actor, CustomCheckFun ) when is_function(CustomCheckFun) ->
    apply_to_intersecting_actor( World, Actor, CustomCheckFun, fun(_A) -> ok end ).

%%          apply_to_intersecting_actors
%% 
%% @doc Finds all actors that intersect the given actor, and then applies the function to them.
%% If 1 or more actors are found to intersect the given actor, then this will
%% return true. Otherwise it will return false.
%%
%% The result of the OnCollFun is ignored.
%% 
%% @spec apply_to_intersecting_actors( World::world(), Actor::actor(), OnCollFun ) -> IsIntersection::boolean()
%%      OnCollFun = ( OtherActorState::actor_state() ) -> term()
apply_to_intersecting_actors( World, Actor, OnCollFun ) ->
    apply_to_intersecting_actors( World, Actor, fun(_A) -> true end, OnCollFun ).

%%          apply_to_intersecting_actors
%% 
%% @doc Only intersects against actors with the given name stored in their actor_state.
%% Only other actors who's actor_name matches the name given will be run against
%% the given OnCollFun.
%% 
%% @spec apply_to_intersecting_actors( World::world(), Actor::actor(), OtherActorName::atom(), OnCollFun ) -> IsIntersection::boolean()
%%      OnCollFun = ( OtherActorState::actor_state() ) -> term()
apply_to_intersecting_actors( World, Actor, OtherActorName, OnCollFun ) when is_atom(OtherActorName) ->
    apply_to_intersecting_actors( World, Actor, OtherActorName, fun(_OAS) -> true end, OnCollFun );

%%          apply_to_intersecting_actors
%%
%% @doc Finds all actors that intersect the given actor, and then applies the function to them.
%% If 1 or more actors are found to intersect the given actor and pass the
%% CustomCheck Fun then this will return true. Otherwise it will return false.
%%
%% The result of the OnCollFun is ignored, and the result of the CustomCheckFun
%% should be true of false.
%% 
%% @spec apply_to_intersecting_actors( World::world(), Actor::actor(), CustomCheckFun, OnCollFun ) -> IsIntersection::boolean()
%%      CustomCheckFun = ( OtherActorState::actor_state() ) -> boolean()
%%      OnCollFun = ( OtherActorState::actor_state() ) -> term()
apply_to_intersecting_actors( World, Actor, CustomCheckFun, OnCollFun ) when is_function(CustomCheckFun) and is_function(OnCollFun) ->
    data_server:send( World, ?WORLD_APPLY_TO_INTERSECTING_ACTORS_MESSAGE_NO_NAME, {self(), Actor, CustomCheckFun, OnCollFun} ),
    receive
        {?COLLISION_APPLY_TO_INTERSECTING_ACTORS_RESPONSE, IsIntersection} -> IsIntersection
    end.

%%          apply_to_intersecting_actors
%%
%% @doc The full apply_to_intersecting_actors function.
%% This includes options for setting a custom checking fun, a fun to be run when
%% a collision is found and a given name of the actors to search through.
%% @spec apply_to_intersecting_actors( World::world(), Actor::actor(), OtherActorName::atom(), CustomCheckFun, OnCollFun ) -> IsIntersection::boolean()
%%      CustomCheckFun = ( OtherActorState::actor_state() ) -> boolean()
%%      OnCollFun = ( OtherActorState::actor_state() ) -> term()
apply_to_intersecting_actors( World, Actor, OtherName, CustomCheckFun, OnCollFun ) when is_function(CustomCheckFun) and is_function(OnCollFun) ->
    data_server:send( World, ?WORLD_APPLY_TO_INTERSECTING_ACTORS_MESSAGE, {self(), Actor, OtherName, CustomCheckFun, OnCollFun} ),
    receive
        {?COLLISION_APPLY_TO_INTERSECTING_ACTORS_RESPONSE, IsIntersection} -> IsIntersection
    end.

%%          apply_to_intersecting_actor
%%
%% @doc The same as the equivalent apply_to_intersecting_actors, only this collides with only one actor.
%% @spec apply_to_intersecting_actor( World::world(), Actor::actor(), OnCollFun ) -> IsIntersection::boolean()
%%      OnCollFun = ( OtherActorState::actor_state() ) -> term()
apply_to_intersecting_actor( World, Actor, OnCollFun ) ->
    apply_to_intersecting_actor( World, Actor, fun(_A) -> true end, OnCollFun ).

%%          apply_to_intersecting_actor
%% 
%% @doc Intersects against the first actor whose name matches the given name.
%% The name used in the test is taken from the name stored in the actor_state
%% from the actor being tested.
%% 
%% @spec apply_to_intersecting_actor( World::world(), Actor::actor(), OtherActorName::atom(), OnCollFun ) -> IsIntersection::boolean()
%%      OnCollFun = ( OtherActorState::actor_state() ) -> term()
apply_to_intersecting_actor( World, Actor, OtherActorName, OnCollFun ) when is_atom(OtherActorName) ->
    apply_to_intersecting_actor( World, Actor, OtherActorName, fun(_OAS) -> true end, OnCollFun );

%%          apply_to_intersecting_actor
%%
%% @doc The same as apply_to_intersecting_actors, only this stops once it has found one intersecting actor.
%% @spec apply_to_intersecting_actor( World::world(), Actor::actor(), CustomCheckFun, OnCollFun ) -> IsIntersection::boolean()
%%      CustomCheckFun = ( OtherActorState::actor_state() ) -> boolean()
%%      OnCollFun = ( OtherActorState::actor_state() ) -> term()
apply_to_intersecting_actor( World, Actor, CustomCheckFun, OnCollFun ) when is_function(CustomCheckFun) and is_function(OnCollFun) ->
    data_server:send( World, ?WORLD_APPLY_TO_INTERSECTING_ACTOR_MESSAGE_NO_NAME, {self(), Actor, CustomCheckFun, OnCollFun} ),
    receive
        {?COLLISION_APPLY_TO_INTERSECTING_ACTOR_RESPONSE, IsIntersection} ->
            IsIntersection
    end.

%%          apply_to_intersecting_actor
%%
%% @doc The full apply_to_intersecting_actor function.
%% This includes options for setting a custom checking fun, a fun to be run when
%% a collision is found and a given name of the actors to search through.
%% @spec apply_to_intersecting_actor( World::world(), Actor::actor(), OtherActorName::atom(), CustomCheckFun, OnCollFun ) -> IsIntersection::boolean()
%%      CustomCheckFun = ( OtherActorState::actor_state() ) -> boolean()
%%      OnCollFun = ( OtherActorState::actor_state() ) -> term()
apply_to_intersecting_actor( World, Actor, OtherName, CustomCheckFun, OnCollFun )
        when is_atom(OtherName) and is_function(CustomCheckFun) and is_function(OnCollFun) ->
    data_server:send( World, ?WORLD_APPLY_TO_INTERSECTING_ACTOR_MESSAGE, {self(), Actor, OtherName, CustomCheckFun, OnCollFun} ),
    receive
        {?COLLISION_APPLY_TO_INTERSECTING_ACTOR_RESPONSE, IsIntersection} ->
            IsIntersection
    end.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API - Super: Actor
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          act
%%
%% @equiv actor:act( Actor, Parent )
act(Actor, Parent)      -> actor:act( Actor, Parent ).

%%          send_act
%%
%% @equiv actor:send_act( Actor, Parent )
send_act(Actor, Parent) -> actor:send_act( Actor, Parent ).

%%          receive_act
%%
%% @equiv actor:receive_act()
receive_act()           -> actor:receive_act().

%%          paint
%%
%% @equiv actor:paint( Actor, G )
paint(Actor, G)         -> actor:paint( Actor, G ).

%%          get_name
%%
%% @equiv actor:get_name( Actor )
get_name(Actor)         -> actor:get_name( Actor ).

%%          get_state
%%
%% @equiv actor:get_state( Actor )
get_state(Actor)        -> actor:get_state( Actor ).

%%          get_act
%%
%% @equiv actor:get_act( Actor )
get_act(Actor)          -> actor:get_act( Actor ).

%%          get_paint
%%
%% @equiv actor:get_paint( Actor )
get_paint(Actor)        -> actor:get_paint( Actor ).

%%          compare
%%
%% @equiv actor:compare( ActorA, ActorB )
compare(ActorA, ActorB) -> actor:compare( ActorA, ActorB ).

%%          get_controls
%%
%% @equiv actor:get_controls( Actor )
get_controls(Actor)     -> actor:get_controls( Actor ).
