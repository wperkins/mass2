# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Fri Jan 10 09:25:43 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2_v027

EPSFILES = \
	plot-courant.eps \
	plot-froude.eps \
	plot-depth.eps \
	plot-elev.eps \
	plot-vel.eps

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	stage.dat \
	grid.dat \
	gage_control.dat \
	mass2_v027.cfg 

all: $(EPSFILES)

$(EPSFILES): plot.nc

clean::	
	rm -f $(EPSFILES)

plot.nc gage.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

clean::
	rm -f plot.nc gage.nc

%.eps: %.gp
	gnuplot $< > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f hotstart*
