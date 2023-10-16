% boid_calc,pl

% returns a delta vector thats pointing away from all other in separation distance radius
calculate_separation_vector(BoidID, (TotalDX, TotalDY, TotalDZ)) :-
    separation_distance(SeparationDistance),
    boid_cell(BoidID,CellID),
    boid(BoidID,(PX, PY, PZ),_),
    findall(
        (DX, DY, DZ),
        (
            boid_cell(M,CellID), % for all boids in cell
            M = boid(_,(OPX,OPY,OPZ),_),
            distance(PX, PY, PZ, OPX, OPY, OPZ, Distance),
            Distance < SeparationDistance, % if too close
            DX is PX - OPX,
            DY is PY - OPY,
            DZ is PZ - OPZ
        ),
        VectorsPointingAway
    ),
    % sum of vecctors
    foldl(add_vectors, VectorsPointingAway, (0, 0, 0), (TotalDX, TotalDY, TotalDZ)).


% returns an average velvector in alignment distance radius
calculate_alignment_vector(cone(p(PX,PY,PZ),_), OtherBoids, t(TotalDX, TotalDY, TotalDZ)) :- 
    alignment_distance(ADistance),
    findall(
        (VX,VY,VZ),
        (
            member(cone(p(OPX,OPY,OPZ),t(VX,VY,VZ)), OtherBoids),
            distance(PX, PY, PZ, OPX, OPY, OPZ, Distance),
            Distance < ADistance % if too close
        ),
        VectorsInRange
    ),
    length(VectorsInRange,N),
    foldl(add_vectors, VectorsInRange, (0, 0, 0), (DX,DY,DZ)),
    TotalDX = DX / N,
    TotalDY = DY / N,
    TotalDZ = DZ / N.

% += (xpos_avg - boid.x)*centeringfactor
calculate_cohesion_vector(cone(p(PX,PY,PZ),_), OtherBoids, t(TotalDX, TotalDY, TotalDZ)) :- 
    alignment_distance(ADistance),
    findall(
        (OPX, OPY, OPZ),
        (
            member(cone(p(OPX,OPY,OPZ),_), OtherBoids),
            distance(PX, PY, PZ, OPX, OPY, OPZ, Distance),
            Distance < ADistance % if too close
        ),
        VectorsInRange
    ),
    length(VectorsInRange,N),
    foldl(add_vectors, VectorsInRange, (0, 0, 0), (DX,DY,DZ)),
    TotalDX = DX / N,
    TotalDY = DY / N,
    TotalDZ = DZ / N.