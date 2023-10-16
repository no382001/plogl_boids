% boid_op.pl

generate_random_boid :-
    map_size(S),
    HalfS is S / 2,
    HalfSm is - HalfS,
    random(HalfSm, HalfS, Px), random(HalfSm,HalfS, Py), random(HalfSm,HalfS, Pz),
    random(-0.01, 0.01, X), random(-0.01, 0.01, Y), random(-0.01, 0.01, Z),
    atomic_list_concat(['boid', X], '_', BoidID),
    assertz(boid(BoidID,(Px,Py,Pz),(X,Y,Z))),
    assign_boid_to_cell(BoidID).

move_with_vector(BoidID) :-
    %format("~w\n",[BoidID]),
    
    boid(BoidID,(X,Y,Z),(TX,TY,TZ)),
    %calculate_separation_vector(BoidID,(SX, SY, SZ)),
    %calculate_alignment_vector(BoidID,(AX, AY, AZ)),
    %calculate_cohesion_vector(BoidID,(CX, CY, CZ)),

    avoidfactor(AF),
    matchingfactor(MF),
    centeringfactor(CF),
    % calculate new position
    %NX is (AX - X) * MF + X + TX + (SX * AF) + (CX - X) * CF,
    %NY is (AY - Y) * MF + Y + TY + (SY * AF) + (CY - Y) * CF,
    %NZ is (AZ - Z) * MF + Z + TZ + (SZ * AF) + (CZ - Z) * CF,

    NX is X + TX,
    NY is Y + TY,
    NZ is Z + TZ,

    map_size(S),
    HalfS is S / 2,

    % check if the new position is beyond half the map size and adjust vectors
    adjust_vector_based_on_position(NX, HalfS, AdjustedNX, TX, AdjustedTX),
    adjust_vector_based_on_position(NY, HalfS, AdjustedNY, TY, AdjustedTY),
    adjust_vector_based_on_position(NZ, HalfS, AdjustedNZ, TZ, AdjustedTZ),

    retract(boid(BoidID,_,_)),
    assertz(
        boid(
            BoidID,
            (AdjustedNX,AdjustedNY,AdjustedNZ),
            (AdjustedTX,AdjustedTY,AdjustedTZ))).

adjust_vector_based_on_position(Pos, HalfMapSize, AdjustedPos, OriginalTranslation, AdjustedTranslation) :-
    % adjust position and reverse the translation
    (   Pos > HalfMapSize
    ->  AdjustedPos is HalfMapSize,  % keep on the boundary
        AdjustedTranslation is -OriginalTranslation % reverse direction
    ;   Pos < -HalfMapSize
    ->  AdjustedPos is -HalfMapSize,
        AdjustedTranslation is -OriginalTranslation
    ;   % default
        AdjustedPos is Pos,
        AdjustedTranslation is OriginalTranslation
    ).


distance(PX, PY, PZ, OtherPX, OtherPY, OtherPZ, Distance) :-
    DX is OtherPX - PX,
    DY is OtherPY - PY,
    DZ is OtherPZ - PZ,
    Distance is sqrt(DX*DX + DY*DY + DZ*DZ).

add_vectors((DX, DY, DZ), (AccDX, AccDY, AccDZ), (NewAccDX, NewAccDY, NewAccDZ)) :-
    NewAccDX is AccDX + DX,
    NewAccDY is AccDY + DY,
    NewAccDZ is AccDZ + DZ.