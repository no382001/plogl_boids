% w - move the camera up
keyboard(119,_,_) :-
    retract(camera_rotation(XAngle, YAngle)),
    NewXAngle is XAngle + 0.1,
    assert(camera_rotation(NewXAngle, YAngle)).

% a - move the camera left
keyboard(97,_,_) :-
    retract(camera_rotation(XAngle, YAngle)),
    NewYAngle is YAngle - 0.1,
    assert(camera_rotation(XAngle, NewYAngle)).

% s - move the camera down
keyboard(115,_,_) :-
    retract(camera_rotation(XAngle, YAngle)),
    NewXAngle is XAngle - 0.1,
    assert(camera_rotation(NewXAngle, YAngle)).

% d - move the camera right
keyboard(100,_,_) :-
    retract(camera_rotation(XAngle, YAngle)),
    NewYAngle is YAngle + 0.1,
    assert(camera_rotation(XAngle, NewYAngle)).

% e - zoom in
keyboard(101,_,_) :-
    retract(zoom(X)),
    NX is X + 0.1,
    assert(zoom(NX)).

% q - zoom out
keyboard(113,_,_) :-
    retract(zoom(X)),
    NX is X - 0.1,
    assert(zoom(NX)).


% escape
keyboard(27,_,_) :-
	write('Closing Window and Exiting...'),nl,
	glutDestroyWindow.