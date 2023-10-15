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

:- dynamic cone_current_pos/1,cone_target_pos/1,cone_velocity/1,cone_angle/1.
cone_current_pos(pos(0,0,0)).
cone_target_pos(pos(0,0,0)).
cone_angle(angle(0,0,0)).
cone_velocity(0.005).


width(500).
height(500).

clamp(Value, Min, Max, ClampedValue) :-
    ClampedValue is min(max(Value, Min), Max).

move_towards_target :-
    cone_current_pos(pos(CX, CY, CZ)),
    cone_target_pos(pos(TX, TY, TZ)),
    cone_velocity(Velocity),

    % calculate direction to move
    DX is TX - CX,
    DY is TY - CY,
    DZ is TZ - CZ,

    Distance is sqrt(DX*DX + DY*DY + DZ*DZ),
    %format("Distance ~w!\n",[Distance]),
    
    % normalize direction
    (Distance < 0.01
    ->  % if the cone is effectively at its target
        set_new_target
    ;   % otherwise, update its position and orientation
        Yaw is atan2(DX, DZ) * (180 / 3.14159), % convert to degrees
        Pitch is asin(DY / Distance) * (180 / 3.14159), % convert to degrees

        % update cone's angle
        retract(cone_angle(_)),
        assert(cone_angle(angle(Pitch, Yaw, 0.0))),

        % update cone's position
        NX is CX + (DX/Distance) * Velocity,
        NY is CY + (DY/Distance) * Velocity,
        NZ is CZ + (DZ/Distance) * Velocity,
        
        clamp(NX, -2.5, 2.5, ClampedNX),
        clamp(NY, -2.5, 2.5, ClampedNY),
        clamp(NZ, -2.5, 2.5, ClampedNZ),
    
        retract(cone_current_pos(_)),
        assert(cone_current_pos(pos(ClampedNX, ClampedNY, ClampedNZ)))
    ).

set_new_target:-
    random(-2.5, 2.5, X), random(-2.5, 2.5, Y), random(-2.5, 2.5, Z),
    %write("new target!\n"),
    retract(cone_target_pos(_)),
    assert(cone_target_pos(pos(X,Y,Z))).
    

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
    %forall(member(Param, [PX,PY,PZ,X,Y,Z]), (write(Param), write(' '))),write('\n'),
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
    
    % update cone's position
    move_towards_target,
    % draw the cone using the current position
    cone_current_pos(Pos),
    cone_angle(Angle),
    draw_cone(Pos, Angle),
	
    glFlush,
	sleep(20),
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