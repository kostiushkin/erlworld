
%%%             Actor_State
%%%
%%% @author Joseph Lenton
%%% @doc
%%% The actor_state is essentially a map that stores properties to values. What
%%% makes this different to other maps is that it also includes some common
%%% properties that all actors require. These are a name, location and size.
%%%
%%% Extra properties can be added by the user through the set function, and
%%% accessed via the get function.
%%%
%%% By default this will terminate itself when removed from a World. To alter
%%% this behaviour you can redefine it's on remove fun with the 'set_on_remove'
%%% function.
%%% 

-module( actor_state ).
-author("Joseph Lenton").

-include("actor_state.hrl").

-define( ACTOR_STATE_DEFAULT_NAME, actor ).

-export([
        new/0, new/1, new/3, new/4
]).

-export([
        set/2, set/3, get/2,
        set_name/2, get_name/1,
        set_xy/2  , set_xy/3, get_xy/1,
        set_x/2   , get_x/1,
        set_y/2   , get_y/1,
        move/2, move/3,
        set_size/2, set_size/3, get_size/1,
        set_width/2, get_width/1,
        set_height/2, get_height/1,
        get_actor/1,
        is_intersection/2,
        set_on_remove/2,
        get_on_remove/1
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          new
%%
%% @doc Creates a new Actor located at 0,0 with size 1,1 and the default name actor.
%% @spec new() -> ActorState::actor_state()
new() ->
    new( ?ACTOR_STATE_DEFAULT_NAME, {0, 0}, {1, 1} ).

%%          new
%%
%% @doc Creates a new default actor but with the list of properties given applied to it.
%% @spec new( Properties::[{Property::atom(), Value::term()}] ) -> ActorState::actor_state()
new( Properties ) ->
    new( ?ACTOR_STATE_DEFAULT_NAME, {0, 0}, {1, 1}, Properties ).

%%          new
%%
%% @doc Creates a new actor_state with the given name, location and size.
%% @spec new( Name::atom(), Location::{X::integer(), Y::integer()}, Size::{Width::integer(), Height::integer()} ) -> ActorState::actor_state()
new( Name, Location, Size ) ->
    new( Name, Location, Size, [] ).

%%          new
%%
%% @doc Creates a new actor_state with the given name, location and size; and with the given list of properties set to it.
%% @spec new( Name::atom(), Location::{X::integer(), Y::integer()}, Size::{Width::integer(), Height::integer()}, Properties::[{Property::atom(), Value::term()}] ) -> ActorState::actor_state()
new( Name, Location, Size, Properties ) ->
    AS = util:new_gb_tree(), % this is the structure of a new gb_tree
    set(
        set_on_remove(
            set_size(
                set_xy(
                    set_name( AS, Name ),
                    Location ),
                Size ),
            new_default_on_remove_fun() ),
        Properties ).

%%          new_default_on_remove_fun
%%
%% @doc Returns the default fun to run when the Actor ends. This is to exit.
%% @spec new_default_on_remove_fun() -> OnRemoveFun::() -> term()
new_default_on_remove_fun() -> fun() -> exit( normal ) end.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          set
%%
%% @doc Sets a given list of properties to the given actor state.
%% @spec set( AS::actor_state(), Properties::[{Key::atom(), Value::term()}] ) -> NewAS::actor_state()
set( AS, [                     ] ) -> AS;
set( AS, [{Key, Value} | Values] ) -> set( set(AS, Key, Value), Values ).

%%          set
%%
%% @doc Sets the given key and value to this actor state.
%% @spec set( AS::actor_state(), Key::atom(), Value::term() ) -> NewAS::actor_state()
set( AS, Key, Value ) -> gb_trees:enter( Key, Value, AS ).

%%          get
%%
%% @doc Returns the property requested from this actor state.
%% @spec get( AS::actor_state(), Key::atom() ) -> {value, Value::term()} | none
get( AS, Key ) ->
    case gb_trees:lookup( Key, AS ) of
        {value, Value} ->
            Value;
        none ->
            none
    end.

%%          set_name
%%
%% @doc This sets an identifyer for naming this actor_state, describing what it is representing.
%% For example a ball, paddle, score_board, missile or player.
%% @spec set_name( AS::actor_state(), Name::atom() ) -> NewAS::actor_state()
set_name( AS, Name ) -> set( AS, ?ACTOR_STATE_NAME, Name ).

%%          set_xy
%%
%% @doc This sets a new X and Y location of this actor_state on the screen.
%% @spec set_xy( AS::actor_state(), X::integer(), Y::integer() ) -> NewAS::actor_state()
set_xy( AS, X, Y ) -> set_xy( AS, {X, Y} ).

%%          set_xy
%%
%% @doc This sets a new X and Y location of this actor_state on the screen.
%% @spec set_xy( AS::actor_state(), {X::integer(), Y::integer()} ) -> NewAS::actor_state()
set_xy( AS, {X, Y} ) -> set( AS, ?ACTOR_STATE_LOCATION, {X, Y} ).

%%          set_x
%% 
%% @doc This sets just the X location of this actor_state.
%% @spec set_x( AS::actor_state(), X::integer() ) -> NewAS::actor_state()
set_x( AS, X ) -> set_xy( AS, {X, get_y(AS)} ).

%%          set_y
%%
%% @doc Sets the Y co-ordinate of this actor_state.
%% @spec set_y( AS::actor_state(), Y::integer() ) -> NewAS::actor_state()
set_y( AS, Y ) -> set_xy( AS, {get_x(AS), Y} ).

%%          set_size
%%
%% @doc Sets the size of this actor_state, neither width or height should ever be less than or equal to 0.
%% @spec set_size( AS::actor_state(), Width::integer(), Height::integer() ) -> NewAS::actor_state()
set_size( AS, Width, Height ) -> set_size( AS, {Width, Height} ).

%%          set_size
%%
%% @doc Sets the size of this actor_state, neither width or height should ever be less than or equal to 0.
%% @spec set_size( AS::actor_state(), {Width::integer(), Height::integer()} ) -> NewAS::actor_state()
set_size( AS, {Width, Height} ) ->
    if
        (Width =< 0) or (Height =< 0) ->
            ErrText = io_lib:format( "Both width and height must be greater then 0. Was given width: ~w and height:~w", [Width, Height] ),
            erlang:error( ErrText );
        true -> set( AS, ?ACTOR_STATE_SIZE, {Width, Height} )
    end.

%%          set_width
%%
%% @doc Sets the width for this actor_state, it must be greater then 0.
%% @spec set_width( AS::actor_state(), Width::number() ) -> NewAS::actor_state()
set_width( AS, Width ) -> set_size( AS, {Width, get_height(AS)} ).

%%          set_height
%%
%% @doc Sets the height for this actor_state, it must be greater then 0.
%% @spec set_height( AS::actor_state(), Height::number() ) -> NewAS::actor_state()
set_height( AS, Height ) -> set_size( AS, {get_width(AS), Height} ).

%%          get_width
%%
%% @doc Returns the width of this actor_state.
%% @spec get_width( AS::actor_state() ) -> Width::number()
get_width( AS ) ->
    { Width, _Height } = get_size( AS ),
    Width.

%%          get_height
%%
%% @doc Returns the height of this actor_state.
%% @spec get_height( AS::actor_state() ) -> Height::number()
get_height( AS ) ->
    { _Width, Height } = get_size( AS ),
    Height.

%%          get_name
%%
%% @doc Retrieves the name for this actor_state.
%% @spec get_name( AS::actor_state() ) -> Name::atom()
get_name( AS ) -> get( AS, ?ACTOR_STATE_NAME ).

%%          get_xy
%%
%% @doc retrieves the X and Y location of this actor_state on teh screen.
%% @spec get_xy( AS::actor_state() ) -> {X::integer(), Y::integer()}
get_xy( AS ) -> get( AS, ?ACTOR_STATE_LOCATION ).

%%          get_x
%%
%% @doc Returns the X location of this actor_state.
%% @spec get_x( AS::actor_state() ) -> X::integer()
get_x(AS) ->
    {X, _Y} = get_xy(AS),
    X.

%%          get_y
%%
%% @doc Returns the Y location of this actor_state.
%% @spec get_y( AS::actor_state() ) -> Y::float()
get_y(AS) ->
    {_X, Y} = get_xy(AS),
    Y.

%%          move
%%
%% @doc Returns the given actor_state moved by the given amount.
%% This is essentially the same as getting the X and Y locations out yourself
%% and then resetting them.
%%
%% @spec move( AS::actor_state(), {MoveX::float(), MoveY::float()} ) -> NewAS::actor_state()
move( AS, {MoveX, MoveY} ) -> move( AS, MoveX, MoveY ).

%%          move
%%
%% @doc Returns the given actor_state moved by the given amount.
%% The same as the other move function, only this doesn't use a tuple.
%%
%% @spec move( AS::actor_state(), MoveX::float(), MoveY::float() ) -> NewAS::actor_state()
move( AS, MoveX, MoveY ) ->
    {X, Y} = get_xy(AS),
    set_xy( AS, X+MoveX, Y+MoveY ).

%%          get_size
%%
%% @doc Returns the width and height of this actor.
%% @spec get_size( AS::actor_state() ) -> {Width::integer(), Height::integer()}
get_size( AS ) -> get( AS, ?ACTOR_STATE_SIZE ).

%%          get_actor
%% 
%% @doc Returns the process id of the last Actor that this was set to.
%% @spec get_actor( AS::actor_state() ) -> Actor::actor()
get_actor( AS ) -> get( AS, ?ACTOR_STATE_ACTOR ).

%%          set_on_remove
%%
%% @doc Sets a fun which will be run when this is removed from it's World.
%% @spec set_on_remove( AS::actor_state(), Fun::() -> term() ) -> NewAS::actor_state()
set_on_remove( AS, Fun ) -> set( AS, ?ACTOR_STATE_ON_REMOVE, Fun ).

%%          get_on_remove
%%
%% @doc Returns the fun that will be run when the actor is removed from the World.
%% @spec get_on_remove( AS::actor_state() ) -> Fun::() -> term()
get_on_remove( AS ) -> get( AS, ?ACTOR_STATE_ON_REMOVE ).

%%          is_intersection
%%
%% @doc Returns the result of a simple box shaped bounds check between the two actor_states.
%% The box used to compare is based on the location and size stored in the
%% actor_states.
%% @spec is_intersection( AS::actor_state(), OtherAS::actor_state() ) -> IsIntersection::boolean()
is_intersection( AS, OtherAS ) ->
    {X1, Y1} = get_xy( AS ),
    {X2, Y2} = get_xy( OtherAS ),
    {W1, H1} = get_size( AS ),
    {W2, H2} = get_size( OtherAS ),

    XMin = erlang:max( X1-W1/2, X2-W2/2 ),
    XMax = erlang:min( X1+W1/2, X2+W2/2 ),

    YMin = erlang:max( Y1-H1/2, Y2-H2/2 ),
    YMax = erlang:min( Y1+H1/2, Y2+H2/2 ),
    
    not ( (YMin >= YMax) or (XMin >= XMax) ).
