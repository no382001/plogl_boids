cross_product(vector(X1, Y1, Z1), vector(X2, Y2, Z2), vector(X, Y, Z)) :-
    X is Y1*Z2 - Z1*Y2,
    Y is Z1*X2 - X1*Z2,
    Z is X1*Y2 - Y1*X2.

normalize_vector(vector(0.0, 0.0, 0.0), vector(0.0, 0.0, 0.0)) :- !.
normalize_vector(vector(X, Y, Z), vector(NX, NY, NZ)) :-
    Len is sqrt(X*X + Y*Y + Z*Z),
    Len > 0.0,   % Make sure the length is non-zero
    NX is X / Len,
    NY is Y / Len,
    NZ is Z / Len.


compute_orientation(PointA, PointB, 0.0, 0.0) :- PointA = PointB, !.

compute_orientation(point(X1, Y1, Z1), point(X2, Y2, Z2), Yaw, Pitch) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    DZ is Z2 - Z1,
    normalize_vector(vector(DX, DY, DZ), vector(DXN, DYN, DZN)),
    Yaw is atan2(DXN, DZN) * (180 / 3.14159),
    Pitch is asin(DYN) * (180 / 3.14159).
