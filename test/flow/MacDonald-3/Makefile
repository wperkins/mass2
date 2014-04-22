# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: 2014-04-22 07:56:37 d3g096
# -------------------------------------------------------------
# $Id$

MPIRUN = mpirun
NPROC = 4
MODEL = ../../../mass2

EPSFILES = \
	plot-depth.eps \
	plot-elev.eps

MODELFILES = \
	mass2.cfg \
	mass2.petscrc \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	stage.dat \
	gage_control.dat

all: $(EPSFILES)

$(EPSFILES): plot000.cgns

.PHONY: run

plot000.cgns: run
grid.cgns: run

run: $(MODELFILES) $(MODEL)
	$(MPIRUN) -np $(NPROC) $(MODEL)

%.eps: %.gp
	gnuplot $< > $@

clean:: 
	rm -f *~
	rm -f error-warning*.out mass_source_monitor.out output*.out status*.out
	rm -f gridplot1.dat 
	rm -f plot000.cgns grid.cgns
	rm -f $(EPSFILES)
