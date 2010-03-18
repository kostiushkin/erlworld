
%%% 
%%%             collisions
%%% 
%%% @author Joseph Lenton
%%% @doc This defines the interface for the collisions modules.
%%% How they actually is entirely down to the implementor, this just defines
%%% the protocols and some boiler plate to connect the update, add, remove and
%%% collision detection functions to your code.
%%% 
%%% To implement you pass in functions that define the actions to perform when
%%% those protocols are run. This is literally just a series of funs.
%%% 
%%% It is intended that the user will never use this directly. Instead the World
%%% will pass calls on to this automatically at the appropriate time.
%%% 
%%% All of the messages are one way messages where the response is to be
%%% performed by the fun provided. This allows the funs provided to pass work
%%% out to seperate processes.
%%%
%%% Implementors should also take care to ensure that implementing collisions
%%% modules do not include actors intersecting against themselves (which should
%%% always be true).
%%%

-module( collisions ).
-author("Joseph Lenton").

-include("collisions.hrl").

-define( ALL_ACTORS, 0 ). % all this has to be is a non-atom

-export([
        new/6
]).

-export([
        add/2,
        remove/2,
        remove_all/1,
        apply_to_intersecting_actors/5, apply_to_intersecting_actors/6,
        apply_to_intersecting_actor/5, apply_to_intersecting_actor/6
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          new
%%
%% @doc This creates a new Collisions object defined by the functions given.
%% The state is the internal state of the collisions object. The funs provided
%% implement each of the functions required by the collisions object.
%%
%% To implement a new type of collisions you only need to call this function and
%% pass in the required values. No more then that.
%% 
%% @spec new( State::term(), AddFun, RemoveFun, RemoveAllFun, ApplyToIntersectingActorsFun, ApplyToIntersectingActorFun ) -> Collisions::collisions()
new( State, AddFun, RemoveFun, RemoveAllFun, ApplyToIntersectingActorsFun, ApplyToIntersectingActorFun ) ->
    function_server:new(
            State,
            [
                    { ?COLLISION_ADD_MESSAGE        , AddFun       },
                    { ?COLLISION_APPLY_TO_INTERSECTING_ACTORS_MESSAGE,
                        fun( Params, FunState ) ->
                            run_coll(
                                    Params, FunState,
                                    ?COLLISION_APPLY_TO_INTERSECTING_ACTORS_RESPONSE,
                                    ApplyToIntersectingActorsFun )
                        end
                    },
                    { ?COLLISION_APPLY_TO_INTERSECTING_ACTOR_MESSAGE,
                        fun( Params, FunState ) ->
                            run_coll(
                                    Params, FunState,
                                    ?COLLISION_APPLY_TO_INTERSECTING_ACTOR_RESPONSE,
                                    ApplyToIntersectingActorFun )
                        end
                    }
            ],
            [
                    { ?COLLISION_REMOVE_MESSAGE,
                        fun( Params, FunState ) ->
                            { RemoveFun(Params, FunState)   , ok }
                        end },
                    { ?COLLISION_REMOVE_ALL_MESSAGE,
                        fun( Params, FunState ) ->
                            { RemoveAllFun(Params, FunState), ok }
                        end }
            ] % no requests
    ).

run_coll( {Sender, OtherParams}, FunState, ResponseMessage, CollisionLogicFun ) ->
    spawn( fun() ->
        Sender ! { ResponseMessage, CollisionLogicFun(OtherParams, FunState) }
    end ),
    FunState.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          add
%%
%% @doc This is called to add an actor to the collisions object.
%% Any future check for an intersection should now include the actor given.
%% @spec add( Colls::collisions(), Actor::actor() ) -> ok
add( Colls, Actor )    -> data_server:send( Colls, ?COLLISION_ADD_MESSAGE, Actor ).

%%          remove
%%
%% @doc This is called whenever an Actor is to be removed from the collisions object.
%% From now on when an actor is checked for collisions, the actor provided should
%% not be included as a part of the intersecting actors.
%% @spec remove( Colls::collisions(), Actor::actor() ) -> ok
remove( Colls, Actor ) -> data_server:request( Colls, ?COLLISION_REMOVE_MESSAGE, Actor ).

%%          remove_all
%%
%% @doc This removes all actors stored inside of this collisions object.
%% @spec remove_all( Colls::collisions() ) -> ok
remove_all( Colls ) -> data_server:request( Colls, ?COLLISION_REMOVE_ALL_MESSAGE ).

apply_to_intersecting_actors( Colls, Sender, Actor, CheckFun, OnCollFun ) ->
    apply_to_intersecting_actors( Colls, Sender, ?ALL_ACTORS, Actor, CheckFun, OnCollFun ).

%%          apply_to_intersecting_actors
%%
%% @doc This will search for all actors that intersect the given actor.
%% All of the intersecting actors will be run on the OnCollFunc, and then a true
%% or false will be sent back to the Sender.
%% Note that this returns directly and the result will be sent to the Sender
%% process id.
%% @spec( Colls::collisions(), Sender::pid(), OtherName::atom(), Actor::actor(), CheckFun, OnCollFun ) -> ok
%%      where CheckFun::fun( OtherActorState::actor_state() ) -> IsIntersection::boolean()
%%            OnCollFun::fun( OtherActorState::actor_state() ) -> term()
apply_to_intersecting_actors( Colls, Sender, OtherName, Actor, CheckFun, OnCollFun ) ->
    data_server:send( Colls, ?COLLISION_APPLY_TO_INTERSECTING_ACTORS_MESSAGE, {Sender, {OtherName, Actor, CheckFun, OnCollFun}} ).

apply_to_intersecting_actor( Colls, Sender, Actor, CheckFun, OnCollFun ) ->
    apply_to_intersecting_actor( Colls, Sender, ?ALL_ACTORS, Actor, CheckFun, OnCollFun ).

%%          apply_to_intersecting_actor
%%
%% @doc This will search for one actors that intersect the given actor.
%% The intersecting actor will be passed into the OnCollFunc, and then a true
%% or false will be sent back to the Sender.
%% Note that this returns directly and the result will be sent to the Sender
%% process id.
%% @spec( Colls::collisions(), Sender::pid(), OtherName::atom(), Actor::actor(), CheckFun, OnCollFun ) -> ok
%%      where CheckFun::fun( OtherActorState::actor_state() ) -> IsIntersection::boolean()
%%            OnCollFun::fun( OtherActorState::actor_state() ) -> term()
apply_to_intersecting_actor( Colls, Sender, OtherName, Actor, CheckFun, OnCollFun ) ->
    data_server:send( Colls, ?COLLISION_APPLY_TO_INTERSECTING_ACTOR_MESSAGE, {Sender, {OtherName, Actor, CheckFun, OnCollFun}} ).
