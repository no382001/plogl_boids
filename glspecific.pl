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