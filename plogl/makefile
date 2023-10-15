SOBJ=   lib/plOpenGL.so
CC=swipl-ld
CFLAGS=-shared -ggdb -Wall -D__USE_FIXED_PROTOTYPES__ -ansi -v
LDFLAGS=-lGL -lglut -lGLU -lm

all:    $(SOBJ)

OBJ=	c/plOpenGL.o

$(SOBJ): $(OBJ)
	mkdir -p lib
	$(CC) $(CFLAGS) $(LDSOFLAGS) -o $@ $(OBJ) $(SWISOLIB) $(LDFLAGS)