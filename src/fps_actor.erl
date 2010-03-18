
%%%         fps_actor
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This is a helper actor for use with debugging. When it acts it will update
%%% it's frame count. If a second has gone past then it will print out the
%%% currrent frame count to the terminal.
%%%
%%% It's printing will occur in it's paint function to ensure it does not
%%% interfear with any other actors.
%%%

-module( fps_actor ).
-author("Joseph Lenton").

%% denotes that there is no FPS value
-define( NO_FPS, -1 ).

-export([
        new/0
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          new
%% 
%% @doc Creates a new FPS Actor.
%% @spec new() -> FPS::actor()
new() ->
    actor:new(
            new_act(),
            new_paint(),
            new_state()
    ).

%%          new_state
%%
%% @doc Creates the initial state for the FPS actor.
%% @spec new_state() -> State::actor_state()
new_state() -> set_fps( actor_state:new(), 0, 0, ?NO_FPS ).

%%          set_fps
%%
%% @doc Sets the fps values to the given actor_state.
%% @spec set_fps( State::actor_state(), FPS::integer(), SwitchTime::number(), PrintFPS::integer() ) -> NewState::actor_state()
set_fps( State, FPS, SwitchTime, PrintFPS ) ->
    actor_state:set(
        State,
        [ { fps       , FPS        },
          { switchTime, SwitchTime },
          { printFPS  , PrintFPS   }  ]
    ).

%%          new_act
%%
%% @doc Creates the act callback function.
%% @spec new_act() -> Act::fun( State::actor_state(), Parent::pid() )
new_act() ->
    fun( State, _Parent) ->
        Now = util:get_time(),
        IsFPSTimeout = (actor_state:get(State, switchTime) < Now),
        if
            IsFPSTimeout ->
                set_fps( State,
                        0,
                        Now + 1000,
                        actor_state:get( State, fps )
                );
            true ->
                set_fps( State,
                        actor_state:get(State, fps) + 1,
                        actor_state:get(State, swtichTime),
                        ?NO_FPS
                )
        end
    end.

%%          new_paint
%%
%% @doc Creates the paint callback function.
%% @spec new_paint() -> Paint::fun( State::actor_state(), G::graphics() )
new_paint() ->
    fun( State, _G ) ->
        case actor_state:get( State, printFPS ) of
            ?NO_FPS ->
                ok;
            FPS ->
                io:format( "fps: ~w~n", [ FPS ] )
        end
    end.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
