# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Tue Feb  5 09:03:23 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = \
	plot-Cn.1.0.eps \
	plot-Cn.6.0.eps \
	plot-Cn.0.1.eps

MODELFILES = \
	bcspecs.dat \
	conc-bc.dat \
	flow.dat \
	gage_control.dat \
	grid.dat \
	mass2_v027.base \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat

all: $(EPSFILES)

plot-Cn.1.0.nc: $(MODELFILES) $(MODEL)
	cp mass2_v027.base mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot-Cn.1.0.nc

plot-Cn.6.0.nc: $(MODELFILES) $(MODEL)
	sed -e '17s/^20.0/120.0/' -e '38s/^36/6 /' -e '39s/^36/6 /' < mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot-Cn.6.0.nc

plot-Cn.0.1.nc: $(MODELFILES) $(MODEL)
	sed -e '17s/^20.0/2.0/' -e '38s/^36/360/' -e '39s/^36/360/' < mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot-Cn.0.1.nc

plot-Cn.1.0.eps: plot.gp plot-Cn.1.0.nc
	sed -e 's/plot\.nc/plot-Cn.1.0.nc/g' plot.gp | gnuplot > $@

plot-Cn.6.0.eps: plot.gp plot-Cn.6.0.nc
	sed -e 's/plot\.nc/plot-Cn.6.0.nc/g' plot.gp | gnuplot > $@

plot-Cn.0.1.eps: plot.gp plot-Cn.0.1.nc
	sed -e 's/plot\.nc/plot-Cn.0.1.nc/g' plot.gp | gnuplot > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat mass2_v027.cfg
	rm -f plot.nc gage.nc plot-Cn.1.0.nc plot-Cn.6.0.nc plot-Cn.0.1.nc
	rm -f batch.log
	rm -f $(EPSFILES)
