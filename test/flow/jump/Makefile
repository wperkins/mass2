# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Wed Feb  6 06:35:45 2002 by William A. Perkins <perk@leechong.pnl.gov>
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
	gage_control.dat

all: $(EPSFILES)

$(EPSFILES): plot.nc.2

plot.nc.2 gage.nc.2: $(MODELFILES) mass2_v027.cfg.2 hotstart.bin $(MODEL)
	cp mass2_v027.cfg.2 mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot.nc.2 

hotstart.bin: $(MODELFILES) mass2_v027.cfg.1 $(MODEL)
	cp mass2_v027.cfg.1 mass2_v027.cfg
	$(MODEL)
	mv hotstart_04-01-1996_180000.bin hotstart.bin

%.eps: %.gp
	gnuplot $< > $@


clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat mass2_v027.cfg
	rm -f plot.nc.1 plot.nc.2 gage.nc
	rm -f hotstart*
	rm -f $(EPSFILES)