# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Fri Feb  1 08:00:06 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = \
	plot-depth.eps \
	plot-elev.eps \
	plot-vel.eps \
	plot.eps

MODELFILES = \
	bcspecs.dat \
	zero.dat \
	mass2_v027.cfg \
	stage.dat

all: $(EPSFILES)

$(EPSFILES): plot.nc


plot.nc: $(MODELFILES)
	$(MODEL)

plot.eps: plot.lay plot.nc
	$(TECPLOT) -b -y plot.eps plot.lay -p ../../../tecplot/macros/export_eps.mcr

%.eps: %.gp
	gnuplot $< > $@


clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat gage_control.dat
	rm -f plot.nc 
	rm -f batch.log
	rm -f $(EPSFILES)

