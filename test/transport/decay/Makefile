# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 14, 2002 by William A. Perkins
# Last Change: Thu Feb 14 06:49:08 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = \
	plot-conc.eps

MODELFILES = \
	bcspecs.dat \
	conc-bc.dat \
	flow.dat \
	gage_control.dat \
	grid.dat \
	mass2_v027.cfg \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat

all: $(EPSFILES)

plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

plot-conc.eps: plot-conc.gp plot.nc

%.eps: %.gp
	gnuplot $< > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc gage.nc 
	rm -f $(EPSFILES)
