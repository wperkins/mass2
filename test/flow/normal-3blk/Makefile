# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Thu Feb  1 19:15:15 2007 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2

EPSFILES = \
	plot-depth.eps \
	plot-depth-all.eps \
	plot-elev.eps \
	plot-vel.eps \
	plot-depth-metric.eps \
	plot-elev-metric.eps \
	plot-vel-metric.eps

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	mass2_v027.cfg \
	stage.dat \
	grid1.dat grid2.dat grid3.dat

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