# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Thu May  8 14:03:50 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES = \
	mixing.eps \
	mixing.cgns.eps \
	plot-lat-1.eps \
	plot-lat-2.eps \
	plot-long.eps \

MODELFILES = \
	bkgnd.dat \
	conc.dat \
	flow.dat \
	grid1.dat \
	grid2.dat \
	mass2_v027.cfg \
	stage.dat \
	scalar_bcspecs.dat \
	scalar_source.dat \
	bcspecs.dat

all: $(EPSFILES)

# -------------------------------------------------------------
# the base case -- comparison to analytic solution
# -------------------------------------------------------------
plot.nc plot000.cgns: $(MODELFILES) $(MODEL)
	$(MODEL)

clean:: 
	rm -f plot.nc plot000.cgns stuff_balance.out

mixing.eps: plot.nc mixing.lay
	$(TECPLOT) -b -y $@ mixing.lay -p ../../../tecplot/macros/export_eps.mcr

mixing.cgns.eps: plot000.cgns mixing.cgns.lay
	$(TECPLOT) -b -y $@ mixing.cgns.lay -p ../../../tecplot/macros/export_eps.mcr

plot-lat-1.eps: plot-lat-1.gp ../mixing/solution.pl plot.nc
	cat plot-lat-1.gp | gnuplot > $@

plot-lat-2.eps: plot-lat-2.gp ../mixing/solution.pl plot.nc
	cat plot-lat-2.gp | gnuplot > $@

plot-long.eps: plot-long.gp ../mixing/solution.pl plot.nc
	cat plot-long.gp | gnuplot > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc 
	rm -f batch.log
	rm -f $(EPSFILES)
