# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 19, 2002 by William A. Perkins
# Last Change: 2014-06-04 13:48:28 d3g096
# -------------------------------------------------------------

MPIRUN = mpirun
NPROC = 4
MODEL = ../../../mass2

EPSFILES = \
	plot-sed.eps \
	plot-conc.eps \
	plot-smass.eps \
	plot-mass.eps

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	mass2.cfg \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat \
	stuff-bc.dat \
	zero.dat

all: $(EPSFILES)

clean:: 
	rm -f $(EPSFILES)

$(EPSFILES): plot000.cgns

%.eps: %.gp
	gnuplot $< > $@

plot000.cgns grid.cgns: run

run: $(MODELFILES) $(MODEL)
	$(MPIRUN) -np $(NPROC) $(MODEL)
.PHONY: run

clean::
	rm -f plot000.cgns grid.cgns 

clean::
	rm -f *~
	rm -f error-warning*.out mass_source_monitor.out output*.out status*.out
	rm -f gridplot1.dat
	rm -f hotstart*.bin
