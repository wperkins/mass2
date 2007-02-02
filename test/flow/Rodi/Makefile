# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 23, 2004 by William A. Perkins
# Last Change: Mon Apr 19 12:10:31 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES =										\
	streamlines.eps								\
	streamlines-metric.eps						\
	vectors.eps									\
	vectors-metric.eps

MODELFILES = \
	bcspecs.dat \
	main_flow.dat \
	jet_flow.dat \
	grid.dat \
	mass2_v027.cfg \
	stage.dat \
	gage_control.dat

all: $(EPSFILES)

streamlines.eps: plot.nc streamlines.lay
	$(TECPLOT) -b -y $@ streamlines.lay -p ../../../tecplot/macros/export_eps.mcr

vectors.eps: plot.nc vectors.lay
	$(TECPLOT) -b -y $@ vectors.lay -p ../../../tecplot/macros/export_eps.mcr

streamlines-metric.eps: plot.nc streamlines-metric.lay
	$(TECPLOT) -b -y $@ streamlines-metric.lay -p ../../../tecplot/macros/export_eps.mcr

vectors-metric.eps: plot.nc vectors-metric.lay
	$(TECPLOT) -b -y $@ vectors-metric.lay -p ../../../tecplot/macros/export_eps.mcr
plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

metric.plt: plot.nc ../spur-dike-1/process.mcr
	$(TECPLOT) -b -p ../spur-dike-1/process.mcr

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc 
	rm -f batch.log
	rm -f $(EPSFILES)
