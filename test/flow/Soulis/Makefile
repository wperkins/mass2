# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 23, 2004 by William A. Perkins
# Last Change: Fri Jan 23 11:48:25 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL=../../../mass2
TECPLOT=tecplot10

EPSFILES= \
	plot2u.eps \
	plot2d.eps \
	plot5u.eps \
	plot5d.eps

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	mass2_v027.cfg \
	stage.dat \
	gage_control.dat

all: $(EPSFILES)

$(EPSFILES): plot.nc

plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

%.eps: %.gp 
	gnuplot $< > $@

grid.dat: mkgrid.pl
	perl $< > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc 
	rm -f batch.log
	rm -f $(EPSFILES)
