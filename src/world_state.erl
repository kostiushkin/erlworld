
%%%             world_state
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This is for holding and representing the data inside of a World actor. It
%%% has functions for altering and retrieving the data inside of this state.
%%%
%%% Essentially the WorldState itself is just a data structure and this module
%%% includes functions for working on that data structure, abstracting you away
%%% from how it is implemented.
%%%
%%% The paint order numbers refer to when an actor will be painted. The numbers
%%% themselves mean nothing, it's how they relate to each other that creates the
%%% order. Actors will be drawn in an order from the lowest paint order to the
%%% highest. You can use any number for their paint order.
%%%
%%% For example an actor set with the paint order -100 will be drawn before one
%%% with -20. If you wish to draw anything before the first actor then you will
%%% need to add it and give it a paint order lower then -100, say -101.
%%% 
%%% Actors with the same paint order should be assumed to be drawn in a constant
%%% random order. Essentially that actors will be drawn in the same random order
%%% on everyframe unless some of those actors are removed and then readded to
%%% the world_state.
%%%

-module(world_state).
-author("Joseph Lenton").

-include("actor.hrl").
-include("world_state.hrl").

-define( DEFAULT_NAME, world ).

-export([
        new/0, new/1, new/2
]).

-export([
        map_actors/2,
        
        add_actor/2, add_actor/3,
        add_actors/2,
        remove_actor/2, remove_actors/2,
        remove_all_actors/1,

        act_actors/1, paint_actors/2
]).

-export([
        apply_to_intersecting_actors/5, apply_to_intersecting_actors/6,
        apply_to_intersecting_actor/5, apply_to_intersecting_actor/6
]).

-export([
        set/2, set/3, get/2,
        get_actor/1,
        get_actors/1,
        set_name/2, get_name/1,
        set_xy/2  , set_xy/3, get_xy/1,
        set_x/2   , get_x/1,
        set_y/2   , get_y/1,
        move/2    , move/3,
        set_size/2, set_size/3, get_size/1,
        set_width/2, get_width/1,
        set_height/2, get_height/1,
        set_on_remove/2, get_on_remove/1,
        set_paint_order/3,
        get_paint_order/2,
        get_number_actors/1
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          new
%%
%% @doc Creates a new empty world.
%% @spec new() -> WorldState
new() ->
    new( [] ).

%%          new
%%
%% @doc Creates a new WorldState that holds the given actors and the given State of the world.
%% @spec new( [Actor::actor()] ) -> WorldState::world_state()
new( Actors ) ->
    new( Actors, [] ).

%%          new
%%
%% @doc A new WorldState which is initialized with the given properties and stores the given actors.
%% @spec new( [Actor::actor()], [{Property::atom(), Value::term()}] ) -> WorldState::world_state()
new( Actors, Properties ) ->
    % you set properties last to allow them to override the actors and collisions, if they wish too
    set(
            add_actors(
                    set_actors_internal(
                            set(
                                    set_name(
                                            set_collisions( actor_state:new(), collisions_list:new() ),
                                            ?DEFAULT_NAME ),
                                    [
                                        { ?WORLD_STATE_ACTORS_PAINT_NUMS, dict:new() },
                                        { ?WORLD_STATE_NUM_ACTORS       , 0  },
                                        { ?WORLD_STATE_ADD_REMOVE_ACTORS, [] }
                                    ]),
                            util:new_gb_tree()
                    ),
                    Actors
            ), Properties
    ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          map_actors
%%
%% @doc Runs the given function with each actor in this world_state passed into it.
%% This running is performed in the painting order.
%%
%% @spec map_actors( WS::world_state(), Fun::(Actor::actor()) -> term() ) -> ok
map_actors( WS, Fun ) ->
    Actors = get_actors_internal( WS ),
    map_actors_outer( Fun, gb_trees:iterator(Actors) ),
    ok.

map_actors_outer( Fun, Iter ) ->
    case gb_trees:next(Iter) of
        {_Key, Actors, Iter2} ->
            map_actors_inner( Fun, gb_sets:iterator(Actors) ),
            map_actors_outer( Fun, Iter2 );
        none ->
            ok
    end.

map_actors_inner( Fun, Iter ) ->
    case gb_sets:next(Iter) of
        {Actor, Iter2} ->
            Fun( Actor ),
            map_actors_inner( Fun, Iter2 );
        none -> ok
    end.

%%          get_actors
%%
%% @doc Retrieves all the actors held inside this world state. An empty list is returned if there are no actors.
%% @spec get_actors( WorldState::world_state() ) -> [Actor::actor()]
get_actors( WS ) ->
    { Actors, _PaintNums } = lists:unzip( dict:to_list( get_paint_nums(WS) ) ),
    Actors.

%%          get_actors_internal
%%
%% @doc Returns the internal tree used for storing all of the actors in the World.
%% For internal use only as this exposes the internal implementation of the
%% WorldState.
%%
%% @spec get_actors_internal( WS::world_state() ) -> gb_trees()
get_actors_internal( WS ) ->
    get( WS, ?WORLD_STATE_ACTORS ).

%%          set_actors_internal
%%
%% @doc Sets the actors held inside this world to those given.
%% @spec set_actors_internal( WorldState::world_state(), Actors::gb_trees() ) -> NewState::world_state()
set_actors_internal( WS, As ) ->
    set( WS, ?WORLD_STATE_ACTORS, As ).

get_paint_nums( WS ) ->
    get( WS, ?WORLD_STATE_ACTORS_PAINT_NUMS ).

set_paint_nums( WS, Nums ) ->
    set( WS, ?WORLD_STATE_ACTORS_PAINT_NUMS, Nums ).

%%          get_number_actors
%%
%% @doc Returns the number of actors stored in this world_state.
%% @spec get_number_actors( WS::world_state() ) -> integer()
get_number_actors( WS ) ->
    get( WS, ?WORLD_STATE_NUM_ACTORS ).

get_collisions( WS ) ->
    get( WS, ?WORLD_STATE_COLLISIONS ).

set_collisions( WS, Coll ) ->
    set( WS, ?WORLD_STATE_COLLISIONS, Coll ).

%%              set_paint_order
%%
%% @doc Sets when this Actor will be painted.
%% The order is relative to the other actors stored. By default an Actor's paint
%% order is 0 from when it has been entered into the world.
%%
%% The actor must be already present in this world for it's paint order to be
%% set.
%% 
%% @spec set_paint_order( WS::world_state(), Actor::actor(), Order::integer() ) -> NewWS::world_state()
set_paint_order( WS, Actor, Order ) ->
    {NewWS, OldOrder} = swap_actor_paint_order( WS, Actor, Order ),
    add_actor_internal( remove_actor_internal( NewWS, Actor, OldOrder ), Actor, Order ).

%%              get_paint_order
%%
%% @doc Returns the painting order number for the Actor given in this world.
%% If the actor is not present then the default painting order, 0, is returned
%% instead.
%%
%% @spec get_paint_order( WS::world_state(), Actor::actor() ) -> PaintOrder::integer()
get_paint_order( WS, Actor ) ->
    case dict:find( Actor, get_paint_nums(WS) ) of
            {ok, Order} -> Order;
             error      -> 0
    end.

%%              swap_actor_paint_order
%%
%% @doc Stores the given Actor under the given order and returns it's old order.
%% A new world_state is also returned to replace the one given.
%%
%% If the actor was not previously stored in the given world state then 0 is
%% returned as it's old order number.
%%
%% @spec swap_actor_paint_order( WS::world_state(), Actor::actor(), Order::integer() ) -> { NewWS::world_state(), OldOrder::integer() }
swap_actor_paint_order( WS, Actor, Order ) ->
    PaintNums = get_paint_nums( WS ),
    OldOrder = case dict:find( Actor, PaintNums ) of
        { ok, Value } -> Value;
        error -> 0
    end,
    { set_paint_nums(WS, dict:store(Actor, Order, PaintNums)), OldOrder }.

%%              add_actors
%%
%% @doc Returns a world_state with the given list of actors added to it.
%% @spec add_actors( WorldState::world_state(), Actors::[actor()] ) -> NewWorldState::world_state()
add_actors( WS, [] ) -> WS;
add_actors( WS, [Actor | AS] ) -> add_actors( add_actor(WS, Actor), AS ).

%%          add_actor
%%
%% @doc Adds the given actor to this world state so it is now stored.
%% The actor will be added with the paint order of 0.
%%
%% @spec add_actor(WorldState::world_state(), Actor::actor()) -> NewWorldState::world_state()
add_actor( WS, Actor ) ->
    add_actor( WS, Actor, 0 ).

%%          add_actor
%%
%% @doc Adds an actor to the given world_state with the given paint order.
add_actor( WS, Actor, PaintOrder ) ->
    collisions:add( get_collisions(WS), Actor ),
    NewWS = add_actor_internal( WS, Actor, PaintOrder ),
    set( NewWS, ?WORLD_STATE_NUM_ACTORS, get_number_actors(WS)+1 ).

%%          add_actor_internal
%%
%% @doc Internal function for setting an actor to this world_state with the given order number.
%%
add_actor_internal( WS, Actor, PaintOrder ) ->
    Actors = get_actors_internal( WS ),
    ActorsSet = case gb_trees:lookup( PaintOrder, Actors ) of
            { value, TempActorsSet } -> TempActorsSet;
            none                     -> gb_sets:new()
    end,
    NewActorsSet = gb_trees:enter( PaintOrder, gb_sets:add(Actor, ActorsSet), Actors ),
    NewWS = set_paint_nums( WS, dict:store( Actor, PaintOrder, get_paint_nums(WS)) ),
    set_actors_internal( NewWS, NewActorsSet ).

%% 		remove_actor
%%
%% @doc Removes the given actor from this world state.
%% @spec remove_actor(WS::world_state(), Actor::actor()) -> NewWorldState::world_state()
remove_actor( WS, Actor ) ->
    PaintNums = get_paint_nums( WS ),
    case dict:find( Actor, PaintNums ) of
        { ok, PaintOrder } ->
            NewPaintNums = dict:erase( Actor, PaintNums ),
            collisions:remove( get_collisions(WS), Actor ),
            NewWS = set_paint_nums(
                    remove_actor_internal( WS, Actor, PaintOrder ),
                    NewPaintNums ),
            data_server:send( Actor, ?ACTOR_MESSAGE_RUN_ON_REMOVE ),
            set( NewWS, ?WORLD_STATE_NUM_ACTORS, get_number_actors(WS)-1 );
        error ->
            WS
    end.

remove_actor_internal( WS, Actor, PaintOrder ) ->
    Actors = get_actors_internal( WS ),
    ActorSet = gb_trees:get(PaintOrder, Actors),
    NewActors = gb_trees:enter(
            PaintOrder,
            gb_sets:delete( Actor, ActorSet ),
            Actors
    ),
    set_actors_internal( WS, NewActors ).

%%              remove_actors
%%
%% @doc Returns a world_state with the given list of actors removed from to it.
%% @spec remove_actors( WS::world_state(), Actors::[actor()] ) -> NewWorldState::world_state()
remove_actors( WS, [] ) -> WS;
remove_actors( WS, [Actor | AS] ) -> remove_actors( remove_actor(WS, Actor), AS ).

%%      remove_all_actors
%%
%% @doc Removes all of the actors from this world state returning one which contains none.
%% @spec remove_all_actors( WS::world_state() ) -> WorldState::world_state()
remove_all_actors( WS ) ->
    Actors = get_actors_internal( WS ),
    remove_all_actors_outer( WS, gb_trees:iterator(Actors) ).

remove_all_actors_outer( WS, Iter ) ->
    case gb_trees:next(Iter) of
        {_Key, Actors, Iter2} ->
            NewWS = remove_all_actors_inner( WS, gb_sets:iterator(Actors) ),
            map_actors_outer( NewWS, Iter2 );
        none ->
            WS
    end.

remove_all_actors_inner( WS, Iter ) ->
    case gb_sets:next(Iter) of
        {Actor, Iter2} ->
            NewWS = remove_actor( WS, Actor ),
            remove_all_actors_inner( NewWS, Iter2 );
        none ->
            WS
    end.

%%      act_actors
%%
%% @doc Tells all of the actors inside of this world state to each update.
%% @spec act_actors( WorldState::world_state() ) -> WorldState::world_state()
act_actors( WorldState ) ->
    Self   = get_actor( WorldState ),
    NumActors = get_number_actors( WorldState ),
    map_actors(
          WorldState,
          fun( Actor ) -> actor:send_act( Actor, Self ) end
    ),
    receive_acts( NumActors ),
    WorldState.

%%      paint_actors
%%
%% @doc Tells all of the actors inside of this world state to each paint.
%% @spec paint_actors( WorldState::world_state(), G::graphics() ) -> ok
paint_actors( WorldState, G ) ->
    map_actors( WorldState,
            fun( Actor ) ->
                actor:paint( Actor, G )
            end
    ).

%%      receive_acts
%%
%% @doc Helper function to grab all of the receive_act responses from the actors.
%% The number given states how many actors to wait for. If this is bigger then
%% the number it should be waiting for then it will block forever.
%%
%% @spec receive_acts( Num::integer() ) -> ok
receive_acts( 0 ) -> ok;
receive_acts( Num ) ->
    actor:receive_act(),
    receive_acts( Num-1 ).

%%      apply_to_intersecting_actors
%%
%% @doc Passes the call to check for a collision onto it's stored collisions object.
%% @spec apply_to_intersecting_actors( WState::world_state(), Sender::pid(), Actor::actor(), CheckFun, OnCollFun ) -> IsIntersection::boolean()
%%      CheckFun  = ( OtherState::actor_state() ) -> boolean()
%%      OnCollFun = ( OtherState::actor_state() ) -> term()
apply_to_intersecting_actors( WState, Sender, Actor, CheckFun, OnCollFunc ) ->
    collisions:apply_to_intersecting_actors(
            get_collisions( WState ),
            Sender,
            Actor,
            CheckFun,
            OnCollFunc
    ),
    WState.

apply_to_intersecting_actors( WState, Sender, Actor, OtherName, CheckFun, OnCollFunc ) ->
    collisions:apply_to_intersecting_actors(
            get_collisions( WState ),
            Sender,
            OtherName,
            Actor,
            CheckFun,
            OnCollFunc
    ),
    WState.

%%      apply_to_intersecting_actor
%%
%% @doc Passes the call to check for a collision onto it's stored collisions object.
%% The result will be sent to the
%%
%% @spec apply_to_intersecting_actor( WState::world_state(), Sender::pid(), Actor::actor(), CheckFun, OnCollFun ) -> IsIntersection::boolean()
%%      CheckFun  = ( OtherState::actor_state() ) -> boolean()
%%      OnCollFun = ( OtherState::actor_state() ) -> term()
apply_to_intersecting_actor( WState, Sender, Actor, CheckFun, OnCollFunc ) ->
    collisions:apply_to_intersecting_actor(
            get_collisions( WState ),
            Sender,
            Actor,
            CheckFun,
            OnCollFunc
    ),
    WState.

apply_to_intersecting_actor( WState, Sender, Actor, OtherName, CheckFun, OnCollFunc ) ->
    collisions:apply_to_intersecting_actor(
            get_collisions( WState ),
            Sender,
            OtherName,
            Actor,
            CheckFun,
            OnCollFunc
    ),
    WState.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API - Super Class
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          set
%%
%% @equiv actor_state:set(AS, Name, Value)
set( WS, Name, Value ) -> actor_state:set( WS, Name, Value ).

%%          set
%%
%% @equiv actor_state:set(AS, Properties)
set( WS, Properties ) -> actor_state:set( WS, Properties ).

%%          get
%%
%% @equiv actor_state:get(AS, Name)
get( WS, Name )      -> actor_state:get( WS, Name ).

%%          set_name
%%
%% @equiv actor_state:set_name( AS, Name )
set_name( AS, Name ) -> actor_state:set_name( AS, Name ).

%%          set_xy
%%
%% @equiv actor_state:set_xy( AS, X, Y )
set_xy( AS, X, Y ) -> actor_state:set_xy( AS, X, Y ).

%%          set_xy
%%
%% @equiv actor_state:set_xy( AS, Location )
set_xy( AS, Location ) -> actor_state:set_xy( AS, Location ).

%%          set_x
%%
%% @equiv actor_state:set_x( AS, X )
set_x( AS, X ) -> actor_state:set_x( AS, X ).

%%          set_y
%%
%% @equiv actor_state:set_y( AS, Y )
set_y( AS, Y ) -> actor_state:set_y( AS, Y ).

%%          set_size
%%
%% @equiv actor_state:set_size( AS, X, Y )
set_size( AS, X, Y ) -> actor_state:set_size( AS, X, Y ).

%%          set_size
%%
%% @equiv actor_state:set_size( AS, Size )
set_size( AS, Size ) -> actor_state:set_size( AS, Size ).

%%          set_width
%%
%% @equiv actor_state:set_width( AS, Width )
set_width( AS, Width ) -> actor_state:set_width( AS, Width ).

%%          set_height
%%
%% @equiv actor_state:set_height( AS, Height )
set_height( AS, Height ) -> actor_state:set_height( AS, Height ).

%%          get_width
%%
%% @equiv actor_state:get_width( AS )
get_width(AS) -> actor_state:get_width( AS ).

%%          get_height
%%
%% @equiv actor_state:get_height( AS )
get_height(AS) -> actor_state:get_height( AS ).

%%          get_name
%%
%% @equiv actor_state:get_name( AS )
get_name( AS ) -> actor_state:get_name( AS ).

%%          get_xy
%%
%% @equiv actor_state:get_xy( AS )
get_xy( AS ) -> actor_state:get_xy( AS ).

%%          get_x
%%
%% @equiv actor_state:get_x( AS )
get_x(AS) -> actor_state:get_x( AS ).

%%          get_y
%%
%% @equiv actor_state:get_y( AS )
get_y(AS) -> actor_state:get_y( AS ).

%%          move
%%
%% @equiv actor_state:move( AS, Move )
move(AS, Move) -> actor_state:move( AS, Move ).

%%          move
%%
%% @equiv actor_state:move( AS, X, Y )
move(AS, X, Y) -> actor_state:move( AS, X, Y ).

%%          get_size
%%
%% @equiv actor_state:get_size( AS )
get_size( AS ) -> actor_state:get_size( AS ).

%%          get_actor
%%
%% @equiv actor_state:get_actor( AS )
get_actor( AS ) -> actor_state:get_actor( AS ).

%%          set_on_remove
%%
%% @equiv actor_state:set_on_remove( AS, Fun )
set_on_remove( AS, Fun ) -> actor_state:set_on_remove( AS, Fun ).

%%          get_on_remove
%%
%% @equiv actor_state:get_on_remove( AS )
get_on_remove( AS ) -> actor_state:get_on_remove( AS ).
