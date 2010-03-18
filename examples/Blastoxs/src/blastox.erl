%%%
%%%         blastox
%%%
%%% This is the main starting class of Blastox. To run call blastox:start() in
%%% the erlang shell.
%%%

-module( blastox ).
-author("Joseph Lenton").

-include("blastox.hrl").

-export([ start/0 ]).

%%          start
%% 
%% @doc Starts the SpaceInvaders game.
%% 
%% @spec start() -> ok
start() ->
    Random = rand:new(),
    register( ?RANDOM, Random ),

    Display = display:new( ?WIDTH, ?HEIGHT, ?TITLE ),
    MainLoop = mainloop:new( Display ),
    mainloop:run( MainLoop, game:new(Display) ).
