# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 14, 2002 by William A. Perkins
# Last Change: Tue Feb 19 07:48:06 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = \
	plot-mass.eps

MODELFILES = \
	bcspecs.dat \
	stuff-bc.dat \
	flow.dat \
	grid.dat \
	mass2_v027.cfg \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat

all: $(EPSFILES)

plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

plot-mass.eps: plot-mass.gp plot.nc

%.eps: %.gp
	gnuplot $< > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat gage_control.dat
	rm -f plot.nc gage.nc 
	rm -f $(EPSFILES)
