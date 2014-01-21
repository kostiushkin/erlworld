%%%
%%%         steering
%%%
%%% @author Fabian Hachenberg
%%% @doc
%%% This contains the records for steering interfaces
%%%

%% Center of Circle is determined by Radius * CenterOffFactor
-record(wander_state, {centeroff_factor, radius, delta=math:pi()}).

