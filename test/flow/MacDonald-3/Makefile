# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: 2014-04-03 11:13:41 d3g096
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

$(EPSFILES): plot.nc


plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

%.eps: %.gp
	gnuplot $< > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc 
	rm -f $(EPSFILES)
