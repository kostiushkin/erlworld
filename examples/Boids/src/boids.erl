
%%%-------------------------------------------------------------------
%%%         Boids
%%%
%%% @doc This is an ErlWorld example scenario of Boids moving around
%%% the screen.
%%%
%%% You can add boids by left-clicking anywhere in the window. The
%%% boids will be created in the direction that the mouse was last
%%% moving in. The behaviour of the boids is described here
%%% http://www.red3d.com/cwr/boids/
%%%
%%% The code is based on the "circles" example by Joseph Lenton
%%% @end
%%% @author Fabian Hachenberg - https://github.com/fHachenberg
%%%-------------------------------------------------------------------
-module(boids).
-author("Fabian Hachenberg").

-include_lib("ErlWorld/include/controls.hrl").
-include_lib("ErlWorld/src/steering.hrl").

% values for setting up the display
-define( WIDTH , 800 ).
-define( HEIGHT, 600 ).
-define( TITLE , "Boids" ).

% the name of the random number generator
-define( RANDOM, random_server ).

% values for calculating random numbers
-define( SIZE_MIN , 4  ).
-define( SIZE_MAX , 36 ).

-define( COLOR_MIN, 0.4 ).
-define( COLOR_MAX, 0.8 ).

-define( SPEED_MIN, 1.5 ).
-define( SPEED_MAX, 2.5 ).

% the minimum amount of time that must pass between creating boids, in _microseconds_
-define( MIN_TIMEOUT, 50000 ).

-export([ start/0 ]).

%%
%% @doc Starts the Boids scenario.
start() ->
    Random = rand:new(),
    register( ?RANDOM, Random ),

    Display = display:new( ?WIDTH, ?HEIGHT, ?TITLE ),
    MainLoop = mainloop:new( Display ),
    BoidImg = image:new( Display, "boid.png" ),
    mainloop:run( MainLoop, new_world(BoidImg) ).

%%
%% @doc Creates and returns a World actor ready and setup for running the scenario.
new_world(BoidImg) ->
%    World = world:new(),
    World = world:new(
            fun(State, _P) -> world_state:act_actors(State) end,
            fun(State, G)  ->
                graphics:set_clear_color(G, color:white()),
                world_state:paint_actors(State, G)
            end
    ),
    world:add_actor( World, new_mouse(BoidImg) ),
    World.

%% @doc Determines the boids enclosed in circle {Bx, By}, Radius
get_boids(World, {Bx, By}, Radius) ->
    lists:filtermap(fun(Actor)-> 
            Name = actor:get_name(Actor),
            if Name =:= boid ->                          
                 {X, Y} = actor_state:get_xy(actor:get_state(Actor)),
                 Len = vec2d:norm({Bx-X, By-Y}),
                 if Len < Radius ->
                    {true, Actor};
                 true ->
                    false
                 end;
            true ->
                false
            end
        end, world_state:get_actors(actor:get_state(World))).

%%
%% @doc Creates the mouse actor that will listen to input and create the boids.
new_mouse(BoidImg) ->
    Act = fun( AS, Parent ) ->
        Controls = actor:get_controls( Parent ),
        LastLocation = actor_state:get_xy( AS ),
        Location = controls:get_mouse_xy( Controls ),
        LastTime = actor_state:get( AS, last_time ),
        Now = now(),
        Diff = timer:now_diff( Now, LastTime ),

        NewAS = if
            (Diff > ?MIN_TIMEOUT) ->
                IsButtonPress = controls:is_mouse_down(Controls, ?MOUSE_LEFT),
                add_boid( Parent, BoidImg, Location, LastLocation, IsButtonPress ),
                actor_state:set( AS, last_time, Now );
            true ->
                AS
        end,
        
        actor_state:set_xy(
                actor_state:set( NewAS, last_location, LastLocation ),
                Location ),
        actor_state:set_name(NewAS, boid)
    end,
    Paint = fun (_AS, _G) -> ok end,
    State = actor_state:new([
            { last_location, {0, 0} },
            { last_time    , now()  }
    ]),
    actor:new( Act, Paint, State ).

%%
%% @doc If the mouse is pressed, then this will add the boid to the given world.
add_boid( World, BoidImg,  Location,  LastLocation, true  ) ->
    Boid = new_boid( BoidImg, Location, locations_to_angle(LastLocation, Location) ),
    world:add_actor( World, Boid );
add_boid( _World, _BoidImg, _Location, _LastLocation, false ) -> ok.

%%
%% @doc Converts the two locations given to an angle from the first location to the second.
%% The angle is in radians.
%%
locations_to_angle( {X, Y}, {ToX, ToY} ) -> math:atan2( ToY-Y, ToX-X ).

%% 
%% @doc Returns a new boid actor at the given location moving in the given direction.
new_boid(BoidImg, XY, Angle) ->
    Speed  = random_speed(),
    Radius = random_radius(),
    Color  = { random_color(), random_color(), random_color(), random_color() },
    Size   = {10, 10},%{ Radius*2, Radius*2 },    

    %Neighbour Information Update (VERY expensive) is only done every N steps!
    UpdateInterval=50,
    
    State  = actor_state:new( boid, XY, Size, [
            {orientation, Angle}, %orientation of boid
            {velocity, {Speed, 0.0}} , %Linear and Angular velocity of boid
            {last_time, now()}, %used to adapt physics update to rate of updates
            %aim for even distribution of neighbour updates
            {update, round(rand:random( ?RANDOM, 0, UpdateInterval))}, %neighbour information is very expensive to update
                                                                       %therefore it gets updated only every UpdateInterval steps.
                                                                       %boids get - during initialization - a random initial value
                                                                       %to achieve a even distribution of the updates!
            {neighbs, []}]), %current state of the neighbour information for this boid
    
    %Maximum Linear Velocity and Force
    Max_LinV = 10.0,
    Max_LinF = 1.0,

    %Maximum Angular Velocity and Force
    Max_AngV = 0.1,
    Max_AngF = 1000.0,

    %As long as abs(LinF) or abs(AngF) stays within these boundaries,
    %couse/velocity is not changed!
    Tol_LinF = 1.0,
    Tol_AngF = 5.0,
    
    %this routine implements the boid behaviour, it is periodically called for each boid 
    Act = fun( AS, Parent ) ->
        %determine time passed
        LastTime =  actor_state:get( AS, last_time ),
        Now = now(),
        TimeDiff = timer:now_diff( Now, LastTime ),

        %get actor state details
        {X, Y} = actor_state:get_xy( AS ),
        CAngle = actor_state:get( AS, orientation ),
        {LinV, AngV} = actor_state:get( AS, velocity ),
        {NX, NY} = vec2d:angle_to_dir(CAngle),        
        Update = actor_state:get( AS, update ),

        %every Nth step, update neighbour information
        if Update rem UpdateInterval =:= 0 ->
            Neighbs = lists:filter(fun(Actor) -> actor:get_state(Actor) /= AS end, get_boids(Parent, {X, Y}, 120.0));
        true ->
            Neighbs = actor_state:get( AS, neighbs )
        end,
        
        %update boid steering forces    
        if length(Neighbs) > 0 ->
            Avoid = lists:foldl(fun(Actor, {LinFSum, AngFSum}) -> 
                        {NbX, NbY} = actor_state:get_xy(actor:get_state(Actor)),
                        {LinF, AngF} = steering:steer_to_target_velocity(LinV, CAngle, steering:v_from_flee({X, Y}, {NbX, NbY}, Max_LinV)),
                        {LinFSum + LinF, AngFSum + AngF}
                        end, {0, 0}, Neighbs),
            Align = lists:foldl(fun(Actor, {LinFSum, AngFSum}) ->                         
                        TAngle = actor_state:get(actor:get_state(Actor), orientation),
                        %for some reason, sometimes TAngle is not a number, which leads to badarith later...
                        if is_number(TAngle) ->                       
                            {LinF, AngF} = steering:steer_to_align(CAngle, TAngle);
                        true ->
                            {LinF, AngF} = {0, 0}
                        end,
                        {LinFSum + LinF, AngFSum + AngF}
                        end, {0, 0}, Neighbs),
            %PosSum is sum over all boid positions of neighbours
            PosSum = lists:foldl(fun(Actor, {XSum, YSum}) -> 
                        {NbX, NbY} = actor_state:get_xy(actor:get_state(Actor)),                    
                        {XSum + NbX, YSum + NbY}
                        end, {0, 0}, Neighbs),
            %Center of Gravity
            CoG = {element(1, PosSum)/length(Neighbs), element(2, PosSum)/length(Neighbs)},
            Cohese = steering:steer_to_target_velocity(LinV, CAngle, steering:v_from_seek({X, Y}, {element(1, CoG), element(2, CoG)}));            
        true -> 
            Avoid = {0,0},
            Align = {0,0},
            Cohese = {0,0}
        end,
        %Weights for the different boid behaviour components
        FacAvoid=1.1,
        FacAlign=1.0,
        FacCohese=3.0,
        FacWander=0.1,

        Dt = TimeDiff / 16822.0,  %the factor 16822.0 is rather arbitrarily chosen to scale the variables to a value around 1.0

        %weighted sum of all boid behaviour components
        {AvX, AvY} = Avoid,
        {AlX, AlY} = Align,
        {CoX, CoY} = Cohese,
        {WaX, WaY} = steering:steer_to_wander(#wander_state{radius=100.0, centeroff_factor={0.0, 0.00}, delta=0.03}),
        {LinF, AngF}   = {  AvX*FacAvoid+AlX*FacAlign+CoX*FacCohese+WaX*FacWander,    AvY*FacAvoid+AlY*FacAlign+CoY*FacCohese+WaY*FacWander    },
        %clamp forces accordingly
        %There are two methods used for clamping here:
        %1. forces cannot exceed a given constant (which is different for angular and linear velocity)
        %2. forces are set to 0 if they do _not_ exceed another constant. That way, like human beings, boids have a threshold of sensitivity for steering forces.
        %   Small deviations are not noticed and ignored accordingly. If the forces exceed this threshold of sensitivity they react rather strongly.
        {LinFC, AngFC} = {  steering:threshold_real(steering:clamp_real(LinF,{-Max_LinF, Max_LinF}), {-Tol_LinF, Tol_LinF}),
                            steering:threshold_real(steering:clamp_real(AngF,{-Max_AngF, Max_AngF}), {-Tol_AngF, Tol_AngF})},
        
        %euler integration for angular velocity and angle
        A = 0.5, %Used to mix the new angular velocity with the last one, in order to minimize flickering
        NewAngV = AngV * (1-A) + ( + Dt * AngFC) * A,
        NewAngVC  = steering:clamp_real(NewAngV, {-Max_AngV, Max_AngV}),
        NewAngle = CAngle + Dt * NewAngVC,
        {NewNX, NewNY} = vec2d:angle_to_dir(NewAngle),

        %euler integration for linear velocity and position 
        NewLinV  = steering:clamp_real(LinV + Dt * LinFC, {0, Max_LinV}), %-Max_LinV
        {DeltaX, DeltaY} = {NewNX*NewLinV, NewNY*NewLinV},       
        {NewX, NewY} = {update_pos(X + DeltaX, ?WIDTH), update_pos(Y + DeltaY, ?HEIGHT)},

        %update actor state
        AS_pos_updated = actor_state:set_xy(AS, NewX, NewY),
        AS_neighbs_updated = actor_state:set(AS_pos_updated, neighbs, Neighbs),
        AS_orientation_updated = actor_state:set(AS_neighbs_updated, orientation, NewAngle),
        AS_velocity_updated = actor_state:set(AS_orientation_updated, velocity, {NewLinV, NewAngVC}),    
        AS_update_updated = actor_state:set(AS_velocity_updated, update, (Update+1) rem UpdateInterval),
        actor_state:set(AS_update_updated, last_time, now())
    end,

    Paint = fun( AS, G ) ->
        graphics:set_color( G, Color ),
        graphics:draw_image_rotated( G, BoidImg, actor_state:get_xy(AS), actor_state:get_size(AS), actor_state:get( AS, orientation ) + math:pi()*0.5)
    end,
    
    actor:new( Act, Paint, State ).

%% @doc Update position of boid, toroidal topology
update_pos( Val, Length ) when Val < Length, Val >= 0 -> Val;
update_pos( Val, Length ) when Val >= Length -> Val - Length;
update_pos( Val, Length ) when Val < 0 -> Val + Length.

%% @doc Returns a random number to use for the radius of a boid.
random_radius() -> rand:random( ?RANDOM, ?SIZE_MIN , ?SIZE_MAX  ).

%% @doc Returns a random number to use for random colour components.
random_color()  -> rand:random( ?RANDOM, ?COLOR_MIN, ?COLOR_MAX ).

%% @doc Returns a random number to use for starting boid speeds.
random_speed()  -> rand:random( ?RANDOM, ?SPEED_MIN, ?SPEED_MAX ).
