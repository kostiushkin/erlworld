%%% 
%%%             util
%%%
%%% @author Joseph Lenton
%%% @doc
%%% The Util module is where the utility functions are placed.
%%%

-module( util ).

-export([
    new_gb_tree/0,
    get_time/0,
    get_time_micros/0,
    to_radians/1, to_degrees/1,
    remove_element/2, foreach_count/2,
    for/2, for/3, for/4
]).

%%              new_gb_tree
%% 
%% @doc This is a helper function for creating a new gb_tree.
%% @spec new_gb_tree() -> {0, nil}
new_gb_tree() -> {0, nil}.

%%              get_time_micros
%%
%% @doc Returns the current time in microseconds.
%% Each call to this will return a unique value.
%% @spec get_time_micros() -> Time::number()
get_time_micros() -> get_time_micros( erlang:now() ).
get_time_micros( {MegaSecs, Secs, MicroSecs} ) -> (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

%%              get_time
%%
%% @doc Returns the current time in milliseconds.
%% @spec get_time() -> Time::number()
get_time() -> get_time( erlang:now() ).
get_time( {MegaSecs, Secs, _MicroSecs} ) -> (MegaSecs*1000 + Secs)*1000.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Maths
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%              toRadians
%%
%% @doc Converts the given value from radians to degrees.
%% @spec to_radians( Degrees::float() ) -> Radians::float()
to_radians(Degrees) -> Degrees * (math:pi() / 180.0).

%%              toDegrees
%%
%% @doc Converts the given degrees into radians.
%% @spec to_degrees( Radians::float() ) -> Degrees::float()
to_degrees(Radians) -> Radians * (180.0 / math:pi()).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Iterating
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

remove_element(A, Bs) ->
    remove_element_inner(A, [], Bs).

remove_element_inner(_A, As, []) ->
    As;
remove_element_inner(A, As, [B | Bs]) ->
    if
        A == B  ->  As ++ Bs ;
        true    ->  remove_element_inner(A, [B | As], Bs)
    end.

%%              foreach_count
%% 
%% @doc This is the same as the for each function in the lists module, but it returns the number of elements it iterated over.
%% The result of the function is ignored.
%%
%% @spec foreach_count( Func::(L::term()) -> term(), [Ls] ) -> Count::integer()
foreach_count(F, Ls) ->
    foreach_count(F, 0, Ls).
foreach_count(_F, Count, []) ->
    Count;
foreach_count(F, Count, [L | Ls]) ->
    F(L),
    foreach_count(F, Count+1, Ls).

%% 		for
%% 
%% @doc The for loop applies a function to each value in the loop in turn, until the value reaches max.
%% This defaults the start of the loop to 0 and steps by 1 between each value.
%% 
%% @spec for( Max::integer(), Func::(I::integer()) -> term() ) -> Count::integer()
for(Max, Fun) ->
    for(Max, Fun, 1).
	
%%		for
%% 
%% @doc This for loop will iterate from 0 to max, but increments the value in the iterator by Step amount each time.
%% @spec for( Max::integer(), Func::(I::integer()) -> term(), Step::integer() ) -> Count::integer()
for(Max, Fun, Step) ->
    for(0, Max, Fun, Step).
	
%%		for
%% 
%% @doc This for loop will iterate from min to max where min is inclusive and max is exclusive.
%% It will increment by the value in Step for each iteration.
%% 
%% @spec for( Min::integer(), Max::integer(), Func::(I::integer()) -> term(), Step::integer() ) -> integer()
for(Min, Max, Fun, Step)
    when (Step < 0) -> for_down(Min, Max, Fun, Step) ;
for(Min, Max, Fun, Step)
    when (Step > 0) -> for_up(Min, Max, Fun, Step) ;
for(Min, _Max, _Fun, _Step) ->
    Min .

%%		for_down
%% 
%% @doc An inner version of the for loop desigend specifically to handle iterating negatively, i.e. from 10 to 0.
%% @spec for_down( Min::integer(), Max::integer(), Func::(I::integer()) -> term(), Step::integer() ) -> integer()
for_down(Min, Max, Fun, Step) ->
    for_down(Min, Max, Fun, Step, Min).
for_down(Min, Max, Fun, Step, I)
    when (I > Max) ->
        Fun(I),
        for_down(Min, Max, Fun, Step, I-Step);
for_down(_Min, _Max, _Fun, _Step, I) ->
    I.

%%		for_up
%% 
%% @doc An inner version of the for loop desigend specifically to handle iterating positively, i.e. from 0 to 10.
%% @spec for_up( Min::integer(), Max::integer(), Func::(I::integer()) -> term(), Step::integer() ) -> integer()
for_up(Min, Max, Fun, Step) ->
    for_up(Min, Max, Fun, Step, Min).
for_up(Min, Max, Fun, Step, I)
    when (I < Max) ->
        Fun(I),
        for_up(Min, Max, Fun, Step, I+Step);
for_up(_Min, _Max, _Fun, _Step, I) ->
    I.
