
%%% 
%%%             Collisions List
%%% 
%%% @author Joseph Lenton
%%% @doc A list based collisions object that implements the collisions module.
%%% The functions used in the collisions module can be used for interacting with
%%% this collisions object. This is a simple brute force list based collisions
%%% object, however the checks are performed in their own process allowing
%%% multiple checks to be performed at once.
%%% 

-module( collisions_list ).
-author("Joseph Lenton").

-export([
        new/0
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          new
%%
%% @doc Returns a new list based collisions object.
%% The functions in the collisions module should be used for interacting with
%% this object.
%% @spec new() -> Collisions::collisions()
new() ->
    StartState = util:new_gb_tree(),
    collisions:new(
            StartState,
            % add
            fun( Actor, Actors ) ->
                add( Actor, Actors )
            end,
            % remove
            fun( Actor, Actors ) ->
                remove( Actor, Actors )
            end,
            % remove all
            fun( _NoParams, _Actors ) ->
                util:new_gb_tree()
            end,
            % apply to all intersecting actors
            fun( {OtherName, Actor, CheckFun, OnCollFun}, Actors ) ->
                R = apply_to_all_intersecting_actors(
                        OtherName, Actors, Actor,
                        actor:get_state(Actor),
                        CheckFun, OnCollFun
                ),
                R
            end,
            % apply to one intersecting actor
            fun( {OtherName, Actor, CheckFun, OnCollFun}, Actors ) ->
                R = apply_to_one_intersecting_actor(
                        OtherName, Actors, Actor,
                        actor:get_state(Actor),
                        CheckFun, OnCollFun
                ),
                R
            end
    ).

add( Actor, Actors )    -> map( fun(A, Set) -> gb_sets:add(A, Set)         end, Actor, Actors ).
remove( Actor, Actors ) -> map( fun(A, Set) -> gb_sets:del_element(A, Set) end, Actor, Actors ).

map( Fun, Actor, Actors ) ->
    Name = actor:get_name( Actor ),
    gb_trees:enter( Name, Fun(Actor, get_set(Name, Actors)), Actors ).

get_set( Name, Actors ) ->
    case gb_trees:lookup( Name, Actors ) of
        {value, Set} -> Set;
         none        -> gb_sets:new()
    end.

apply_to_all_intersecting_actors( OtherName, Actors, Actor, AState, CheckFun, OnCollFun )
        when is_atom(OtherName) ->
    apply_to_all_intersecting_actors_set(
            Actor,
            AState,
            CheckFun, OnCollFun,
            false, gb_sets:iterator( get_set(OtherName, Actors) )
    );
apply_to_all_intersecting_actors( _OtherName, Actors, Actor, AState, CheckFun, OnCollFun ) ->
    apply_to_all_intersecting_actors_inner(
            Actor, AState,
            CheckFun, OnCollFun,
            false, gb_trees:iterator( Actors )
    ).

apply_to_all_intersecting_actors_inner( Actor, AState, CheckFun, OnCollFun, IsCollision, Iter ) ->
    case gb_trees:next(Iter) of
        {_Key, ActorSet, Iter2} ->
            apply_to_all_intersecting_actors_set( Actor, AState, CheckFun, OnCollFun, IsCollision, gb_sets:iterator(ActorSet) ),
            apply_to_all_intersecting_actors_inner( Actor, AState, CheckFun, OnCollFun, IsCollision, Iter2 );
        none ->
            IsCollision
    end.

%%          apply_to_all_intersecting_actors_set
%%
%% @doc Performs a check
apply_to_all_intersecting_actors_set( Actor, AState, CheckFun, OnCollFun, IsIntersection, Iter ) ->
    case gb_sets:next(Iter) of
        { OtherActor, Iter2 } ->
            if
                Actor /= OtherActor ->
                    OtherAState = actor:get_state( OtherActor ),
                    IsIntersection2 = actor_state:is_intersection( AState, OtherAState ) and CheckFun(OtherAState),
                    if
                        IsIntersection2 -> OnCollFun( OtherActor );
                        true            -> ok
                    end,
                    apply_to_all_intersecting_actors_set( Actor, AState, CheckFun, OnCollFun, IsIntersection or IsIntersection2, Iter2 );
                true ->
                    apply_to_all_intersecting_actors_set( Actor, AState, CheckFun, OnCollFun, IsIntersection , Iter2 )
            end;
        none ->
            IsIntersection
    end.

apply_to_one_intersecting_actor( OtherName, Actors, Actor, AState, CheckFun, OnCollFun )
        when is_atom( OtherName ) ->
    apply_to_one_intersecting_actor_set(
            Actor, AState,
            CheckFun, OnCollFun,
            gb_sets:iterator( get_set(OtherName, Actors) )
    );
apply_to_one_intersecting_actor( _OtherName, Actors, Actor, AState, CheckFun, OnCollFun ) ->
    apply_to_one_intersecting_actor_inner(
            Actor, AState,
            CheckFun, OnCollFun,
            gb_trees:iterator( Actors )
    ).

apply_to_one_intersecting_actor_inner( Actor, AState, CheckFun, OnCollFun, Iter ) ->
    case gb_trees:next(Iter) of
        {_Key, ActorSet, Iter2} ->
            IsCollision = apply_to_one_intersecting_actor_set( Actor, AState, CheckFun, OnCollFun, gb_sets:iterator(ActorSet) ),
            if
                IsCollision -> true;
                true        -> apply_to_one_intersecting_actor_inner( Actor, AState, CheckFun, OnCollFun, Iter2 )
            end;
        none ->
            false
    end.

%%          apply_to_one_intersecting_actor_set
%% 
%% @doc
apply_to_one_intersecting_actor_set( Actor, AState, CheckFun, OnCollFun, Iter ) ->
    case gb_sets:next(Iter) of
        { OtherActor, Iter2 } ->
            if
                Actor /= OtherActor ->
                    OtherAState    = actor:get_state( OtherActor ),
                    IsIntersection = actor_state:is_intersection( AState, OtherAState ) and CheckFun(OtherAState),
                    if
                        IsIntersection ->
                            OnCollFun( OtherAState ),
                            true;
                        true ->
                            apply_to_one_intersecting_actor_set( Actor, AState, CheckFun, OnCollFun, Iter2 )
                    end;
                true ->
                    apply_to_one_intersecting_actor_set( Actor, AState, CheckFun, OnCollFun, Iter2 )
            end;
        none ->
            false
    end.
