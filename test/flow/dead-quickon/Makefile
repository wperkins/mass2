# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Tue Apr  8 12:00:37 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = plot.eps

MODELFILES = \
	flow.dat \
	stage.dat \
	grid.out

all: $(EPSFILES)

plot.eps: plot.nc dead.lay
	$(TECPLOT) -b -y plot.eps dead.lay -p ../../../tecplot/macros/export_eps.mcr

plot.nc: $(MODELFILES) hotstart.bin mass2_v027-dead.cfg bcspecs-dead.dat $(MODEL)
	cp mass2_v027-dead.cfg mass2_v027.cfg
	cp bcspecs-dead.dat bcspecs.dat
	$(MODEL)

hotstart.bin: $(MODELFILES) mass2_v027-warmup.cfg bcspecs-warmup.dat $(MODEL)
	cp mass2_v027-warmup.cfg mass2_v027.cfg
	cp bcspecs-warmup.dat bcspecs.dat
	$(MODEL)
	rm -f plot.nc
	cp hotstart_04-01-1996_060000.bin hotstart.bin

%.eps: %.gp
	gnuplot $< > $@


clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc hotstart*.bin mass2_v027.cfg bcspecs.dat
	rm -f batch.log
	rm -f $(EPSFILES)