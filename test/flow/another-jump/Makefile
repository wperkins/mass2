# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 26, 2004 by William A. Perkins
# Last Change: Fri Apr  2 08:13:28 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2
TECPLOT = tecplot


EPSFILES = \
	plot-depth-metric.eps

MODELFILES = \
	bcspecs.dat \
	gage_control.dat \
	grid.dat \
	mass2_v027.cfg \
	mkgrid.sh \
	stage-vel.dat \
	vel.dat

all: $(EPSFILES)

$(EPSFILES): plot.nc

plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

%.eps: %.gp
	gnuplot $< > $@

clean::
	rm -f *~
	rm -f hotstart*.bin
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc grid.cgns plot000.cgns
	rm -f batch.log
	rm -f $(EPSFILES)
