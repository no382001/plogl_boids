% boid_calc,pl

% returns a delta vector thats pointing away from all other in same cell
calculate_separation_vector(BoidID, (TotalDX, TotalDY, TotalDZ)) :-
    %separation_distance(SeparationDistance),
    boid_cell(BoidID,CellID),
    boid(BoidID,(PX, PY, PZ),_),
    findall(
        (DX, DY, DZ),
        (
            boid_cell(M,CellID),
            boid(M,(OPX,OPY,OPZ),_),
            BoidID \= M,

            % w. sp partitioning i dont think it makes sense to use separation distance at all
            % just adjust avoid factor
            
            %distance(PX, PY, PZ, OPX, OPY, OPZ, Distance),
            DX is PX - OPX,
            DY is PY - OPY,
            DZ is PZ - OPZ,
            
            % connect same cell boids, w/ red
            glColor3f(1.0, 0.0, 0.0),
            kGL_LINES(LINES),
            glPushMatrix,
                glBegin(LINES),
                    glVertex3f(PX, PY, PZ),
                    glVertex3f(OPX, OPY, OPZ),
                glEnd,
            glPopMatrix,
            glColor3f(1.0, 1.0, 1.0)
        ),
        VectorsPointingAway
    ),
    length(VectorsPointingAway,N),
    (N = 0 -> 
        TotalDX is 0,
        TotalDY is 0,
        TotalDZ is 0
    ;
        foldl(add_vectors, VectorsPointingAway, (0, 0, 0), (TotalDX, TotalDY, TotalDZ))
    ).

% returns an average velvector in alignment distance radius
calculate_alignment_vector(BoidID,(TotalDX, TotalDY, TotalDZ)) :- 
    alignment_distance(ADistance),
    boid_cell(BoidID,CellID),
    boid(BoidID,(PX, PY, PZ),_),
    findall(
        (VX,VY,VZ),
        (
            boid_cell(M,CellID), % for all boids in cell
            M = boid(_,(OPX,OPY,OPZ),_),
            distance(PX, PY, PZ, OPX, OPY, OPZ, Distance),
            Distance < ADistance % if too close
        ),
        VectorsInRange
    ),
    length(VectorsInRange,N),
    (   N > 0
    ->  foldl(add_vectors, VectorsInRange, (0, 0, 0), (DX, DY, DZ)),
        TotalDX is DX / N,
        TotalDY is DY / N,
        TotalDZ is DZ / N
    ;   TotalDX = 0,
        TotalDY = 0,
        TotalDZ = 0
    ).

% += (xpos_avg - boid.x)*centeringfactor
calculate_cohesion_vector(BoidID,(TotalDX, TotalDY, TotalDZ)) :- 
    alignment_distance(ADistance),
    boid_cell(BoidID,CellID),
    boid(BoidID,(PX, PY, PZ),_),
    findall(
        (OPX, OPY, OPZ),
        (
            boid_cell(M,CellID), % for all boids in cell
            M = boid(_,(OPX,OPY,OPZ),_),
            distance(PX, PY, PZ, OPX, OPY, OPZ, Distance),
            Distance < ADistance % if too close
        ),
        VectorsInRange
    ),
    length(VectorsInRange,N),
    (   N > 0
    ->  foldl(add_vectors, VectorsInRange, (0, 0, 0), (DX, DY, DZ)),
        TotalDX is DX / N,
        TotalDY is DY / N,
        TotalDZ is DZ / N
    ;   TotalDX = 0,
        TotalDY = 0,
        TotalDZ = 0
    ).