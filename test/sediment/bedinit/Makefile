# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 14, 2002 by William A. Perkins
# Last Change: 2014-06-11 12:11:04 d3g096
# -------------------------------------------------------------

MPIRUN = mpirun
NPROC = 4
MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES = \
	plot-conc-nodecay.eps \
	plot-conc-decay.eps \

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	mass2.cfg \
	scalar_bcspecs.dat \
	scalar_source.base \
	sediment-bc.dat \
	stage.dat \
	stuff-bc.dat \
	zero.dat

.PHONY: all run-nodecay run-decay

all: run-nodecay run-decay

run-nodecay: $(MODELFILES) $(MODEL)
	cp scalar_source.base scalar_source.dat
	$(MPIRUN) -np $(NPROC) $(MODEL)
	mv plot000.cgns plot-nodecay.cgns

run-decay: $(MODELFILES) $(MODEL)
	sed -e '9s/0.0/1.584E-04/' scalar_source.base > scalar_source.dat
	$(MPIRUN) -np $(NPROC) $(MODEL)
	mv plot000.cgns plot-decay.cgns

clean:: 
	rm -f *~
	rm -f error-warning*.out mass_source_monitor.out output*.out status*.out
	rm -f gridplot1.dat 
	rm -f plot.nc gage.nc plot-nodecay.nc plot-decay.nc 
	rm -f grid.cgns plot000.cgns plot-nodecay.cgns plot-decay.cgns
	rm -f $(EPSFILES)
