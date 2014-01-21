
%%%             Steering
%%%
%%% @author Fabian Hachenberg
%%% @doc
%%% Implementation of steering behaviours for Erlang, see
%%% http://www.red3d.com/cwr/steer/ for an introduction.
%%% The implementation is based on OpenSteer
%%% http://opensteer.sourceforge.net
%%% @end 

-module( steering ).
-author("Fabian Hachenberg").

-include("steering.hrl").

% the name of the random number generator
-define( RANDOM, random_server ).

-export([        
        clamp_real/2,
        gap_real/2,
        v_from_seek/2,
        v_from_flee/3,
        steer_to_target_velocity/3,
        steer_to_align/2,
        steer_to_wander/1
        ]).

%% @doc clamps real to interval [Xmin, Xmax]
clamp_real(X, {Xmin, Xmax}) ->
        max(min(X, Xmax), Xmin).

gap_real(X, {Xgmin, Xgmax}), when  ->
        if X >= Xgmin, X =< Xgmax -> 
            0;
        true -> 
            X
        end.

%% @doc Calculates a force to let A seek B
v_from_seek({Ax, Ay}, {Bx, By}) -> 
        {Bx-Ax, By-Ay}.

%% @doc Calculates a force to let the velocity of A match that of B
v_from_vmatch({Avx, Avy}, {Bvx, Bvy}) ->
        {Bvx-Avx, Bvy-Avy}.

%% @doc Calculates a force to let A flee from B
v_from_flee({Ax, Ay}, {Bx, By}, MaxLinV) ->
        {Sx, Sy} = v_from_seek({Ax, Ay}, {Bx, By}),
        Len = max(0.1, vec2d:norm({Sx, Sy})),
        {-MaxLinV/Len*Sx, -MaxLinV/Len*Sy}.

%Derived Steering Behaviours

%% @doc Calculates linear and angular steering-force for A to reach target velocity
steer_to_target_velocity(Speed, Angle, {Tvx, Tvy}) ->
        {Nx, Ny} = vec2d:angle_to_dir(Angle),
        {Ox, Oy} = vec2d:orthogonal({Nx, Ny}),
        {Avx, Avy} = {Speed*Nx, Speed*Ny},
        Linear = Nx*(Tvx-Avx) + Ny*(Tvy-Avy),
        Angular = Ox*(Tvx-Avx) + Oy*(Tvy-Avy),
        %io:format("~p ~p ~p + ~p ~p -> ~p ~p ~n", [Angle, Avx, Avy, Linear, Angular, Tvx, Tvy]),
        {Linear, Angular}.

%% @doc Calculates angular force to minimize difference between Angle and TAngle (target angle)
steer_to_align(Angle, TAngle) ->
        Diff = TAngle - Angle,
        Pi = math:pi(),
        case Diff of 
            Diff when Diff > Pi ->
                steer_to_align(Angle, TAngle-2.0*math:pi());            
            Diff when Diff < -Pi ->
                steer_to_align(Angle, TAngle+2.0*math:pi());
            _ -> 
                {0, TAngle-Angle}
        end.

%% @doc helper function for wander behaviour
get_point_on_ring(W, Angle) ->
        {CenterX, CenterY} = {element(1, W#wander_state.centeroff_factor) * W#wander_state.radius, element(2, W#wander_state.centeroff_factor) * W#wander_state.radius},
        {CenterX+W#wander_state.radius*math:cos(Angle), CenterY+W#wander_state.radius*math:sin(Angle)}.

%% @doc wander behaviour
steer_to_wander(W) ->
        steer_to_target_velocity(0, 0, v_from_seek({0, 0}, get_point_on_ring(W, rand:random(?RANDOM, -W#wander_state.delta, W#wander_state.delta)))).

        




