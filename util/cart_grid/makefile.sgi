#-------------------------------------------------------------
# file: Makefile
# Makefile for cartgrid 
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------

#f90 cartgrid.f90 -ocartgrid -g3 -Ofast -r10000 -TARG:platform=IP30 -OPT:Olimit=0
#  Bill - I use the above to compile.  I grabbed the things I thought would
#   be needed from a mass2 make, but I'm getting a "makefile:1: *** missing separator.  Stop.
#   error.  CR
F90 = f90
COMPILE.f90 = $(F90) $(F90FLAGS) -c $(DEBUG)
LINK.f90 = $(F90) $(LDFLAGS)
MOD=mod
DEBUG =  -Ofast -g3
FLAGS = $(DEBUG) -r10000 -TARG:platform=IP30 -OPT:Olimit=0 

MAINDEBUG = $(DEBUG)
F90FLAGS = $(FLAGS)
LIBLOC =
LDFLAGS = ${LIBLOC} $(FLAGS)
LIBS = -lfpe

TARGET = cartgrid
SRCS =											\
     cartgrid.f90								\
	 elevtbl.f90 

OBJS = $(SRCS:%.f90=%.o)
$(TARGET): $(OBJS)
	${LINK.f90} ${OBJS} ${LIBS} -o ${TARGET}
clean::
	rm -f ${TARGET}

# dependancies for individual object files
cartgrid.o:cartgrid.f90 elevtbl.o
elevtbl.o: elevtbl.f90

%.o: %.f90
	${COMPILE.f90} $<
tags: TAGS
TAGS: $(SRCS)
	etags $(SRCS)

