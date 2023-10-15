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
:- include("utils.pl").
:- include("logic.pl").
:- include("glspecific.pl").

:- dynamic camera_rotation/1,cone_velocity/1,cones/1.
camera_rotation(0.0).
cone_velocity(0.01).
cones(
    [
        cone(p(1.0,0.0,0.0),t(0.0,0.0,0.0)),
        cone(p(0.0,1.0,0.0),t(0.0,0.0,0.0)),
        cone(p(0.0,0.0,1.0),t(0.0,0.0,0.0)),
        cone(p(0.0,1.0,0.0),t(0.0,0.0,0.0)),
        cone(p(1.0,0.0,0.0),t(0.0,0.0,0.0)),
        cone(p(0.0,1.0,0.0),t(0.0,0.0,0.0))
    ]
).


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
    maplist(move_towards_target,Cones,NCones),
    % draw all cones
    forall(member(C,NCones),draw_cone(C)),
    retract(cones(_)),
    assertz(cones(NCones)),
	
    glFlush,
	sleep(5),
	glutSwapBuffers.