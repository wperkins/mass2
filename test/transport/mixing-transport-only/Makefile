# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Thu Feb  1 19:01:46 2007 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES = \
	mixing.eps \
	plot-lat-1.eps \
	plot-lat-2.eps \
	plot-long.eps \

MODELFILES = \
	bkgnd.dat \
	conc.dat \
	grid.dat \
	scalar_bcspecs.dat \
	scalar_source.dat

all: $(EPSFILES)

# -------------------------------------------------------------
# the base case -- comparison to analytic solution
# -------------------------------------------------------------
hotstart.bin: mass2_v027.base flow.dat stage.dat  bcspecs.dat grid.dat $(MODEL)
	sed -e "7s/^T/F/" -e "11s/^F/T/" mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv hotstart_04-01-1996_060000.bin $@
	rm plot.nc

plot.nc: mass2_v027.base $(MODELFILES) $(MODEL) hotstart.bin
	sed -e "6s/^T/F/" mass2_v027.base > mass2_v027.cfg
	$(MODEL)

clean:: 
	rm -f hotstart.bin mass2_v027.cfg

plot-lat-1.eps: plot-lat-1.gp ../mixing/solution.pl plot.nc
	gnuplot plot-lat-1.gp > $@

plot-lat-2.eps: plot-lat-2.gp ../mixing/solution.pl plot.nc
	gnuplot plot-lat-2.gp > $@

plot-long.eps: plot-long.gp ../mixing/solution.pl plot.nc
	 gnuplot plot-long.gp > $@


mixing.eps: plot.nc mixing.lay
	$(TECPLOT) -b -y $@ mixing.lay -p ../../../tecplot/macros/export_eps.mcr

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc 
	rm -f batch.log
	rm -f $(EPSFILES)
