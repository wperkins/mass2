# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 14, 2002 by William A. Perkins
# Last Change: Fri Feb 15 15:15:38 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2_v027
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

all: $(EPSFILES)

plot-nodecay.nc: $(MODELFILES) $(MODEL)
	cp scalar_source.base scalar_source.dat
	$(MODEL)
	mv plot.nc $@

plot-decay.nc: $(MODELFILES) $(MODEL)
	sed -e '9s/0.0/1.584E-04/' scalar_source.base > scalar_source.dat
	$(MODEL)
	mv plot.nc $@

plot-conc-nodecay.eps: plot-conc.gp plot-nodecay.nc
	sed -e 's/plot\.nc/plot-nodecay.nc/' plot-conc.gp | gnuplot > $@

plot-conc-decay.eps: plot-conc.gp plot-decay.nc
	sed -e 's/plot\.nc/plot-decay.nc/' plot-conc.gp | gnuplot > $@


clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat 
	rm -f plot.nc gage.nc plot-nodecay.nc plot-decay.nc 
	rm -f gage_control.dat
	rm -f $(EPSFILES)
