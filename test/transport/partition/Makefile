# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 14, 2002 by William A. Perkins
# Last Change: 2014-06-04 15:31:42 d3g096
# -------------------------------------------------------------

MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES = \
	plot-conc-nodecay.eps \
	plot-conc-decay.eps \

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	mass2_v027.cfg \
	scalar_bcspecs.dat \
	scalar_source.base \
	sediment-bc.dat \
	stage.dat \
	stuff-bc.dat \
	zero.dat

.PHONY: all nodecay decay

all: $(EPSFILES)

nodecay: $(MODELFILES) $(MODEL)
	cp scalar_source.base scalar_source.dat
	$(MODEL)

decay: $(MODELFILES) $(MODEL)
	sed -e '9s/0.0/1.584E-04/' scalar_source.base > scalar_source.dat
	$(MODEL)

plot-conc-nodecay.eps: nodecay plot-conc.gp 
	sed -e 's/plot\.nc/plot-nodecay.nc/' plot-conc.gp | gnuplot > $@

plot-conc-decay.eps: decay plot-conc.gp
	sed -e 's/plot\.nc/plot-decay.nc/' plot-conc.gp | gnuplot > $@


clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat 
	rm -f plot.nc gage.nc plot-nodecay.nc plot-decay.nc 
	rm -f $(EPSFILES)
