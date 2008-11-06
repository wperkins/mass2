# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Tue Oct 28 10:11:42 2008 by William A. Perkins <d3g096@bearflag.pnl.gov>
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
	conc-bc.dat \
	flow.dat \
	gage_control.dat \
	grid1.dat \
	grid2.dat \
	grid3.dat \
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
	sed -e '19s/^20.0/120.0/' -e '40s/^18/3 /' < mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot-Cn.6.0.nc

plot-Cn.0.1.nc: $(MODELFILES) $(MODEL)
	sed -e '19s/^20.0/2.0/' -e '40s/^18/180/' < mass2_v027.base > mass2_v027.cfg
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
