# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Fri Feb 15 15:14:05 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = \
	plot-mass-bc.eps \
	plot-mass-bed.eps

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	gage_control.dat \
	grid.dat \
	mass2_v027.cfg \
	scalar_bcspecs.base \
	scalar_source.base \
	stage.dat 

BEDFILES = \
	background.dat \
	stuff-list.dat \
	stuff-map-1.dat \
	stuff-map.dat \
	stuff-mass-14.dat

all: $(EPSFILES)

plot.bc.nc gage.bc.nc: $(MODELFILES) $(MODEL) conc.dat
	sed -e 's/@FILE@/conc.dat/' scalar_bcspecs.base > scalar_bcspecs.dat
	cp scalar_source.base scalar_source.dat
	$(MODEL)
	mv gage.nc gage.bc.nc
	mv plot.nc plot.bc.nc

plot.bed.nc gage.bed.nc: $(MODELFILES) $(MODEL) $(BEDFILES)
	sed -e 's/@FILE@/background.dat/' scalar_bcspecs.base > scalar_bcspecs.dat
	sed -e '2s,/,BEDSOURCE "stuff-list.dat" "stuff-map.dat" /,' scalar_source.base > scalar_source.dat
	$(MODEL)
	mv gage.nc gage.bed.nc
	mv plot.nc plot.bed.nc


plot-mass-bc.eps: plot-mass-bc.gp plot.bc.nc
plot-mass-bed.eps: plot-mass-bed.gp plot.bed.nc

%.eps: %.gp
	gnuplot $< > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc gage.nc plot.bed.nc gage.bed.nc plot.bc.nc gage.bc.nc
	rm -f batch.log
	rm -f scalar_bcspecs.dat scalar_source.dat
	rm -f $(EPSFILES)
