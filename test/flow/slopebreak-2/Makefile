# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Thu Jan  8 12:13:19 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2

EPSFILES = \
	plot-depth.eps \
	plot-depth-metric.eps \
	plot-elev.eps \
	plot-elev-metric.eps \
	plot-vel.eps \
	plot-vel-metric.eps

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	mass2_v027.cfg \
	stage.dat

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