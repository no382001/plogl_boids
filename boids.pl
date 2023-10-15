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

:- include("key_events.pl").

:- dynamic camera_rotation/1.
camera_rotation(0.0).

width(500).
height(500).

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

draw_cone(pos(PX,PY,PZ),angle(X,Y,Z)) :-
    glPushMatrix,
        glTranslatef(PX,PY,PZ),
        glRotatef(X,Y,Z,0.0),
        glutWireCone(0.5, 0.5, 3, 1),
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
    
    draw_cone(
        pos(1.0,2.0,1.0),
        angle(30.0,1.0,0.0)),
    draw_cone(
        pos(1.5,3.0,0.0),
        angle(50.0,20.0,0.0)),
	
    glFlush,
	sleep(10),
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