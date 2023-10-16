% sp_partitioning.pl
:- dynamic cell/8,boid/3,boid_cell/2.

map_size(10.0).
num_divisions(5.0).

float_between(Start, End, Step, Value) :-
    Start =< End,
    Value = Start.
float_between(Start, End, Step, Value) :-
    Start =< End,
    Next is Start + Step,
    Next =< End,
    float_between(Next, End, Step, Value).

initialize_cells :-
    num_divisions(Divisions),
    map_size(MapSize),
    CellSize is MapSize / Divisions,
    HalfMapSize is MapSize / 2,
    Between is -HalfMapSize,
    ForLimit is HalfMapSize - CellSize,
    forall(float_between(Between,ForLimit,CellSize,X),
        forall(float_between(Between,ForLimit,CellSize,Y),
            forall(float_between(Between,ForLimit,CellSize,Z),
                (   atomic_list_concat(['cell', X, Y, Z], '_', CellID), % unique id
                    CenterX is X + CellSize / 2,
                    CenterY is Y + CellSize / 2,
                    CenterZ is Z + CellSize / 2,
                    assertz(cell(CellID, CenterX, CenterY, CenterZ, CellSize, CellSize, CellSize))
                )
            )
        )
    ).

find_cell_for_position(X, Y, Z, CellID) :-
    cell(CellID, CenterX, CenterY, CenterZ, Width, Height, Depth),
    X >= CenterX - Width / 2,
    X < CenterX + Width / 2,
    Y >= CenterY - Height / 2,
    Y < CenterY + Height / 2,
    Z >= CenterZ - Depth / 2,
    Z < CenterZ + Depth / 2.

assign_boid_to_cell(BoidID) :-
    boid(BoidID, (X, Y, Z), _),
    find_cell_for_position(X, Y, Z, CellID),
    %retract(boid_cell(BoidID,_)), % fails if not already asserted
    assertz(boid_cell(BoidID, CellID)). % assign

handle_boid_movement(BoidID, NewX, NewY, NewZ) :-
    retract(boid(BoidID, _, V)),
    assertz(boid(BoidID, (NewX, NewY, NewZ), V)),
    
    retract(boid_cell(BoidID, _)),    
    assign_boid_to_cell(BoidID).


debug_draw_cells :-
    forall(
        cell(I,X,Y,Z,W,_,_),
        (
            glPushMatrix,
                glTranslatef(X,Y,Z),
                glutWireCube(W),
            glPopMatrix
        )).