# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Tue Apr  8 12:34:39 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = \
	mixing.eps \
	plot-lat-1.mix.eps \
	plot-lat-2.mix.eps \
	plot-long.mix.eps \
	nomixing.eps \
	plot-lat-1.nomix.eps \
	plot-long.nomix.eps \
	wall.eps \
	plot-lat-1.wall.eps \
	plot-long.wall.eps

MODELFILES = \
	bkgnd.dat \
	conc.dat \
	flow.dat \
	grid.dat \
	mass2_v027.base \
	stage.dat \
	scalar_bcspecs.dat \
	scalar_source.dat

all: $(EPSFILES)

# -------------------------------------------------------------
# the base case -- comparison to analytic solution
# -------------------------------------------------------------
plot.mix.nc: $(MODELFILES) $(MODEL) bcspecs.base
	cp mass2_v027.base mass2_v027.cfg
	cp bcspecs.base bcspecs.dat
	$(MODEL)
	mv plot.nc plot.mix.nc

clean:: 
	rm -f plot.mix.nc mass2_v027.cfg bcspecs.dat

mixing.eps: plot.mix.nc mixing.base.lay
	sed -e 's/plot\.nc/plot.mix.nc/g' mixing.base.lay > mixing.lay
	$(TECPLOT) -b -y $@ mixing.lay -p ../../../tecplot/macros/export_eps.mcr

clean::
	rm -f mixing.lay

plot-lat-1.mix.eps: plot-lat-1.gp solution.pl plot.mix.nc
	sed -e 's/plot\.nc/plot.mix.nc/g' plot-lat-1.gp | gnuplot > $@

plot-lat-2.mix.eps: plot-lat-2.gp solution.pl plot.mix.nc
	sed -e 's/plot\.nc/plot.mix.nc/g' plot-lat-2.gp | gnuplot > $@

plot-long.mix.eps: plot-long.gp solution.pl plot.mix.nc
	sed -e 's/plot\.nc/plot.mix.nc/g' plot-long.gp | gnuplot > $@

# -------------------------------------------------------------
# no mixing with zero diffusivity
# -------------------------------------------------------------
plot.nomix.nc: $(MODELFILES) $(MODEL) bcspecs.base
	sed -e '24s/3.00000/0.00000/' mass2_v027.base > mass2_v027.cfg
	cp bcspecs.base bcspecs.dat
	$(MODEL)
	mv plot.nc plot.nomix.nc

clean:: 
	rm -f plot.nomix.nc mass2_v027.cfg bcspecs.dat

nomixing.eps: plot.nomix.nc mixing.base.lay
	sed -e 's/plot\.nc/plot.nomix.nc/g' mixing.base.lay > nomixing.lay
	$(TECPLOT) -b -y $@ nomixing.lay -p ../../../tecplot/macros/export_eps.mcr

clean::
	rm -f nomixing.lay

plot-lat-1.nomix.eps: plot-lat-1.gp solution.pl plot.nomix.nc
	sed -e 's/plot\.nc/plot.nomix.nc/g' plot-lat-1.gp | gnuplot > $@

plot-long.nomix.eps: plot-long.gp solution.pl plot.nomix.nc
	sed -e 's/plot\.nc/plot.nomix.nc/g' plot-long.gp | gnuplot > $@


# -------------------------------------------------------------
# no mixing with wall
# -------------------------------------------------------------
plot.wall.nc: $(MODELFILES) $(MODEL) bcspecs.wall
	cp mass2_v027.base mass2_v027.cfg
	cp bcspecs.wall bcspecs.dat
	$(MODEL)
	mv plot.nc plot.wall.nc

clean:: 
	rm -f plot.wall.nc mass2_v027.cfg bcspecs.dat

wall.eps: plot.wall.nc mixing.base.lay
	sed -e 's/plot\.nc/plot.wall.nc/g' mixing.base.lay > wall.lay
	$(TECPLOT) -b -y $@ wall.lay -p ../../../tecplot/macros/export_eps.mcr

clean:: 
	rm -f wall.lay

plot-lat-1.wall.eps: plot-lat-1.gp solution.pl plot.wall.nc
	sed -e 's/plot\.nc/plot.wall.nc/g' plot-lat-1.gp | gnuplot > $@

plot-long.wall.eps: plot-long.gp solution.pl plot.wall.nc
	sed -e 's/plot\.nc/plot.wall.nc/g' plot-long.gp | gnuplot > $@


clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc 
	rm -f batch.log
	rm -f $(EPSFILES)
