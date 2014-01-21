
%%%             2d Vector Utilities
%%%
%%% @author Fabian Hachenberg
%%% @doc
%%% Helper code for 2d vector algebra
%%% @end

-module( vec2d ).
-author("Fabian Hachenberg").

-export([   
        clamp/2,     
        orthogonal/1,
        angle_to_dir/1,
        norm/1
        ]).

% vector is scaled so its lengths is <= Lmax
clamp({X, Y}, Lmax) ->
        Len = norm({X, Y}),
        Scale = min(Len, Lmax)/Len,
        {X*Scale, Y*Scale}.

% returns the vector orthogonal to (X,Y)
orthogonal({X,Y}) ->
        {-Y, X}.

% converts angle to normalized vector
angle_to_dir(Angle) ->
        {math:cos(Angle), math:sin(Angle)}.

% length of vector
norm({X, Y}) ->
        math:sqrt(X*X + Y*Y).

% rescales vector to Length 1
normalize({X, Y}) ->
        Len = norm({X, Y}),
        {X/Len, Y/Len}.
