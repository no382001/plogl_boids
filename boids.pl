% defs
:- include("plogl/prolog/plGL_defs.pl").
:- include("plogl/prolog/plGLU_defs.pl").
:- include("plogl/prolog/plGLUT_defs.pl").
% static plogl lib
:- use_foreign_library("plogl/lib/plOpenGL.so").
% plogl modules
:- use_module("plogl/prolog/plOpenGL").
:- use_module("plogl/prolog/plGL").
:- use_module("plogl/prolog/plGLU").
:- use_module("plogl/prolog/plGLUT").

map_size(5.0).
width(500).
height(500). 

:- dynamic camera_rotation/1,cone_velocity/1.
camera_rotation(0.0).
cone_velocity(0.01).

:- dynamic cones/1.
cones([]).


generate_random_boid(cone(p(Px,Py,Pz),t(X,Y,Z))) :-
    map_size(S),
    HalfS is S / 2,
    HalfSm is - HalfS,
    random(HalfSm, HalfS, Px), random(HalfSm,HalfS, Py), random(HalfSm,HalfS, Pz),
    random(-0.01, 0.01, X), random(-0.01, 0.01, Y), random(-0.01, 0.01, Z).


% the camera is angled at the main cube
camera :-
	camera_rotation(Angle),
    NewAngle is Angle + 0.001,
    (NewAngle >= 6.28318 % 2PI
        -> CAngle is 0.0
        ; CAngle is NewAngle
    ),
    retract(camera_rotation(_)),
    assert(camera_rotation(CAngle)),

    Radius = 10.0,
    EyeX is cos(CAngle) * Radius,
    EyeZ is sin(CAngle) * Radius,

	gluLookAt(EyeX,0.0,EyeZ,0.0, 0.0, 0.0, 1.0, 1.0, 0.0).

draw_cone(cone(p(PX,PY,PZ),t(TX,TY,TZ))) :-
    % compute direction vector
    DX is TX - PX,
    DY is TY - PY,
    DZ is TZ - PZ,

    % compute Yaw and Pitch
    Yaw is atan2(DX, DZ) * (180 / 3.14159),
    Pitch is - asin(DY / sqrt(DX*DX + DY*DY + DZ*DZ)) * (180 / 3.14159),

    glPushMatrix,
        glTranslatef(PX,PY,PZ),
        glRotatef(Yaw, 0.0, 1.0, 0.0),   % rotate around Y-axis
        glRotatef(Pitch, 1.0, 0.0, 0.0), % rotate around X-axis
        glutWireCone(0.5, 1.0, 5, 1),
    glPopMatrix.

display:-
    % defs
    kGL_COLOR_BUFFER_BIT(COLOR_BUFFER),
	kGL_DEPTH_BUFFER_BIT(DEPTH_BUFFER),
	glClear(COLOR_BUFFER \/ DEPTH_BUFFER),
	glColor3f(1.0, 1.0, 1.0),
	glLoadIdentity,
    camera,
    % main cube, the camera is angled at
    glutWireCube(5.0),
    
    cones(Cones),
    % update cone's position
    maplist(move_with_vector,Cones,NCones),
    % draw all cones
    forall(member(C,NCones),draw_cone(C)),
    retract(cones(_)),
    assertz(cones(NCones)),
	
    glFlush,
	sleep(5),
	glutSwapBuffers.

init:-
    %defs
    kGL_FLAT(FLAT),
    %gl
    glClearColor(0.0, 0.0, 0.0, 0.0),
    glShadeModel(FLAT).

reshape:-
    % defs
    X is 0,
    Y is 0,
    width(W),
    width(H),
    kGL_PROJECTION(PROJECTION),
    kGL_MODELVIEW(MODELVIEW),
    % gl
    glViewport(X,Y,W,H),
    glMatrixMode(PROJECTION),
    glLoadIdentity,
    glFrustum(-1.0, 1.0, -1.0, 1.0, 1.5, 20.0),
    glMatrixMode(MODELVIEW).

idle :- display.

main:-
    % defs
    width(W),
    height(H),
    kGLUT_SINGLE(SINGLE),
    kGLUT_RGB(RGB),

    length(List, 10),
    maplist(generate_random_boid,List),
    retract(cones(_)),
    assertz(cones(List)),
    
    % gl
    glutInit,
    glutInitDisplayMode(SINGLE \/ RGB),
    glutInitWindowSize(W, H),
    glutInitWindowPosition(0,0),
    glutCreateWindow('boids'),
    init,
    glutDisplayFunc,
    glutIdleFunc(idle),
    glutReshapeFunc,
    glutKeyboardFunc,
    glutMainLoop.

move_with_vector(cone(p(CX,CY,CZ),t(TX, TY, TZ)),NC) :-
    % calculate new position
    NX is CX + TX,
    NY is CY + TY,
    NZ is CZ + TZ,

    % get map size
    map_size(S),
    HalfS is S / 2,

    % check if the new position is beyond half the map size and adjust vectors
    adjust_vector_based_on_position(NX, HalfS, AdjustedNX, TX, AdjustedTX),
    adjust_vector_based_on_position(NY, HalfS, AdjustedNY, TY, AdjustedTY),
    adjust_vector_based_on_position(NZ, HalfS, AdjustedNZ, TZ, AdjustedTZ),

    NC = cone(p(AdjustedNX,AdjustedNY,AdjustedNZ),t(AdjustedTX, AdjustedTY, AdjustedTZ)).

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

% escape
keyboard(27,_,_) :-
	write('Closing Window and Exiting...'),nl,
	glutDestroyWindow.