move_towards_target(cone(p(CX,CY,CZ),t(TX, TY, TZ)),NC) :-
    % calculate direction to move
    DX is TX - CX,
    DY is TY - CY,
    DZ is TZ - CZ,

    Distance is sqrt(DX*DX + DY*DY + DZ*DZ),
    %format("Distance ~w!\n",[Distance]),
    
    % normalize direction
    (Distance < 0.01
    ->  % if the cone is effectively at its target
        get_new_target(NT),
        NC = cone(p(CX,CY,CZ),NT)

    ;   cone_velocity(Velocity),
        % update cone's position
        NX is CX + (DX/Distance) * Velocity,
        NY is CY + (DY/Distance) * Velocity,
        NZ is CZ + (DZ/Distance) * Velocity,
    
        NC = cone(p(NX,NY,NZ),t(TX, TY, TZ))
    ).

get_new_target(t(X,Y,Z)):-
    random(-2.5, 2.5, X), random(-2.5, 2.5, Y), random(-2.5, 2.5, Z).
   