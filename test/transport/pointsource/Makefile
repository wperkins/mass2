# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Tue Mar 19 07:44:53 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = \
	plot-lat.eps \
	plot-long.eps \
	plot-mass.eps \
	plan.eps

MODELFILES = \
	background.dat \
	bcspecs.dat \
	flow.dat \
	gage_control.dat \
	grid.dat \
	mass2_v027.cfg \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat \
	stuff-list.dat \
	stuff-map-1.dat \
	stuff-map.dat \
	stuff-mass.dat


all: $(EPSFILES)

plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

plot-lat.eps: plot-lat.gp solution.pl plot.nc
plot-long.eps: plot-long.gp solution.pl plot.nc
plot-mass.eps: plot-mass.gp plot.nc
plan.eps: plan.lay plot.nc
	$(TECPLOT) -b -y $@ plan.lay -p ../../../tecplot/macros/export_eps.mcr


%.eps: %.gp
	gnuplot $< > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc gage.nc
	rm -f batch.log
	rm -f $(EPSFILES)
