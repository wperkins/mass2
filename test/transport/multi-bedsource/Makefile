# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Fri Feb 15 14:56:58 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES = plot-mass.eps

MODELFILES = \
	background.dat \
	bcspecs.dat \
	flow.dat \
	gage_control.dat \
	grid.dat \
	mass2_v027.cfg \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat 

BEDFILES = \
	morestuff-list.dat \
	morestuff-map-1.dat \
	morestuff-map.dat \
	morestuff-mass-19.dat \
	morestuff-mass-28.dat \
	stuff-list.dat \
	stuff-map-1.dat \
	stuff-map.dat \
	stuff-mass-14.dat \
	stuff-mass-23.dat \


all: $(EPSFILES)

plot.nc gage.nc: $(MODELFILES) $(MODEL)
	$(MODEL)


plot-mass.eps: plot-mass.gp plot.nc

%.eps: %.gp
	gnuplot $< > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc gage.nc
	rm -f batch.log
	rm -f $(EPSFILES)
