# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Thu Mar 25 14:02:51 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES = \
	mixing.eps \
	plot-lat-1.eps \
	plot-lat-2.eps \
	plot-long.eps

MODELFILES = \
	../mixing/bkgnd.dat \
	../mixing/conc.dat \
	../mixing/flow.dat \
	../mixing/stage.dat \
	bcspecs.dat \
	grid.dat \
	mass2_v027.cfg \
	scalar_bcspecs.dat \
	scalar_source.dat

all: $(EPSFILES)

# -------------------------------------------------------------
# the base case -- comparison to analytic solution
# -------------------------------------------------------------
plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

mixing.eps: plot.nc ../mixing/mixing.base.lay
	$(TECPLOT) -b -y $@ -p ../../../tecplot/macros/export_eps.mcr ../mixing/mixing.base.lay

plot-lat-1.eps: plot-lat-1.gp ../mixing/solution.pl plot.nc
	sed -e 's/plot\.nc/plot.nc/g' plot-lat-1.gp | gnuplot > $@

plot-lat-2.eps: plot-lat-2.gp ../mixing/solution.pl plot.nc
	sed -e 's/plot\.nc/plot.nc/g' plot-lat-2.gp | gnuplot > $@

plot-long.eps: plot-long.gp ../mixing/solution.pl plot.nc
	sed -e 's/plot\.nc/plot.nc/g' plot-long.gp | gnuplot > $@


clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc  stuff_balance.out
	rm -f batch.log
	rm -f $(EPSFILES)
