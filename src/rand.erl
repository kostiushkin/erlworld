
%%%-------------------------------------------------------------------
%%%         Rand
%%%
%%% @doc A random number generator server.
%%%
%%% You create a new random server by calling the new function and
%%% then pass this into the random function when generating numbers.
%%%
%%% This is only meant as a cheap and cheerful random number module.
%%% For anything more sophisticated use the random module supplied in
%%% the Erlang OTP.
%%% 
%%% @author Joseph Lenton - JL235@Kent.ac.uk
%%%-------------------------------------------------------------------

-module( rand ).

-author("Joseph Lenton").

-define( RAND_GET_RANDOM, random_get_random ).

-export([
        new/0,
        random/2, random/3,
        destroy/1
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      new
%%
%% @doc Spawns a new random number server and returns it's process id.
%% @spec new() -> Server::pid()
new() ->
    data_server:new(
            fun(_Message, _Params, State ) -> State end,
            fun( Message,  Params, State ) -> on_request(Message, Params, State) end,
            random:seed0()
    ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Message Handling
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      on_requests
%%
%% @doc Deals with generating random numbers on request.
on_request( ?RAND_GET_RANDOM, {Min, Max}, Random ) ->
    {Mult, NewRandom} = random:uniform_s( Random ),
    {NewRandom, (Max-Min)*Mult + Min}.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%      random
%%
%% @doc Returns a number from 0 to Max.
%% @spec random( Rand::pid(), Max::number() ) -> RandomNum::number()
random(Rand, Max) -> random(Rand, 0, Max).

%%      random
%%
%% @doc Returns a number from Min to Max.
%% @spec random( Rand::pid(), Min::number() , Max::number() ) -> RandomNum::number()
random(Rand, Min, Max) -> data_server:request( Rand, ?RAND_GET_RANDOM, {Min, Max} ).

%%      destroy
%%
%% @doc Destroys the given random number generator process.
%% This is the same as just killing it yourself using the exit function.
%%
%% @spec destroy( Rand::pid() ) -> true
destroy(Rand) -> exit(Rand, kill).
