# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Thu Mar 25 15:24:52 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES = \
	plot-Cn.1.0.eps \
	plot-Cn.6.0.eps \
	plot-Cn.0.1.eps

MODELFILES = \
	bcspecs.dat \
	gage_control.dat \
	grid1.dat \
	grid2.dat \
	mass2_v027.base \
	scalar_bcspecs.dat \
	scalar_source.dat \
	../1d-analytic-2blk/conc-bc.dat \
	../1d-analytic-2blk/flow.dat \
	../1d-analytic-2blk/stage.dat

all: $(EPSFILES)

plot-Cn.1.0.nc: $(MODELFILES) $(MODEL)
	cp mass2_v027.base mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot-Cn.1.0.nc

plot-Cn.6.0.nc: $(MODELFILES) $(MODEL)
	sed -e '18s/^20.0/120.0/' -e '38s/^18/3 /' -e '39s/^18/3 /' < mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot-Cn.6.0.nc

plot-Cn.0.1.nc: $(MODELFILES) $(MODEL)
	sed -e '18s/^20.0/2.0/' -e '38s/^18/180/' -e '39s/^18/180/' < mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot-Cn.0.1.nc

plot-Cn.1.0.eps: plot.gp plot-Cn.1.0.nc
	sed -e 's/TheTitle/Courant Number = 1.0/' \
		-e 's/plot\.nc/plot-Cn.1.0.nc/g' plot.gp | gnuplot > $@

plot-Cn.6.0.eps: plot.gp plot-Cn.6.0.nc
	sed -e 's/plot\.nc/plot-Cn.6.0.nc/g' \
	-e 's/TheTitle/Courant Number = 6.0/' plot.gp | gnuplot > $@

plot-Cn.0.1.eps: plot.gp plot-Cn.0.1.nc
	sed -e 's/plot\.nc/plot-Cn.0.1.nc/g' \
	-e 's/TheTitle/Courant Number = 0.1/' plot.gp | gnuplot > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat mass2_v027.cfg
	rm -f plot.nc gage.nc plot-Cn.1.0.nc plot-Cn.6.0.nc plot-Cn.0.1.nc
	rm -f batch.log
	rm -f $(EPSFILES)
