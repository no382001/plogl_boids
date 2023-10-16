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

:- include("boid_op.pl").
:- include("boid_calc.pl").
:- include("sp_partitioning.pl").

% static defs
width(500).
height(500). 

% sim defs
no_boids(20).

% boid behav
separation_distance(0.05).
avoidfactor(1.0).
alignment_distance(0.0005).
matchingfactor(0.0005).
cohesion_distance(0.05).
centeringfactor(1.0).

% dynamic defs
:- dynamic camera_rotation/1,cone_velocity/1,cones/1.
camera_rotation(0.0).
cone_velocity(0.07).
cones([]).

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

% draw cone looking in the direction it is going
draw_cone((PX,PY,PZ),(TX,TY,TZ)) :-
    DX is PX + TX,
    DY is PY + TY,
    DZ is PZ + TZ,

    glPushMatrix,
        glTranslatef(PX,PY,PZ),
        gluLookAt(
            0.0, 0.0, 0.0,  % camera at origin (0,0,0)
            DX, DY, DZ,     % looking tw the direction vector
            0.0, 1.0, 0.0), % up
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
    map_size(S),
    glutWireCube(S),
    
    % update cone's position, and draw
    forall(
        boid(I,_,_),
        move_with_vector(I)),
    
    forall(
        boid(ID,Pos,Vec),
        draw_cone(Pos,Vec)),
    
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

repeat(_,0) :- !.
repeat(P,N) :- 
    P, N1 is N - 1,
    repeat(P,N1), !.

main:-
    % defs
    width(W),
    height(H),
    kGLUT_SINGLE(SINGLE),
    kGLUT_RGB(RGB),
    % generate boids
    setup,

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

% escape
keyboard(27,_,_) :-
	write('Closing Window and Exiting...'),nl,
	glutDestroyWindow.

setup :- 
    initialize_cells,
    no_boids(NOB),
    repeat(generate_random_boid,NOB).