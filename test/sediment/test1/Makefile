# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 19, 2002 by William A. Perkins
# Last Change: 2014-06-08 11:53:29 d3g096
# -------------------------------------------------------------

MPIRUN = mpirun
NPROC = 4
MODEL = ../../../mass2

EPSFILES = \
	plot-sed.eps \
	plot-shear.eps \
	plot-mass.eps

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	mass2.cfg \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat \
	zero.dat

.PHONY: all run

all: run

clean:: 
	rm -f $(EPSFILES)

run: $(MODELFILES) $(MODEL)
	$(MPIRUN) -np $(NPROC) $(MODEL)

clean::
	rm -f plot.nc gage.nc

$(EPSFILES): plot.nc

%.eps: %.gp
	gnuplot $< > $@

clean::
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
