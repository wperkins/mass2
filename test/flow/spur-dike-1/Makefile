# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Thu Apr 22 07:40:36 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2
TECPLOT = tecplot9

EPSFILES = \
	streamlines.eps \
	streamlines-metric.eps \
	vectors.eps \
	vectors-metric.eps \
	long-1.0.eps \
	long-1.5.eps \
	long-2.0.eps \
	long-3.0.eps \
	long-4.0.eps

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	mass2_v027.cfg \
	stage.dat \
	gage_control.dat

all: $(EPSFILES)

streamlines.eps: plot.nc streamlines.lay
	$(TECPLOT) -b -y $@ streamlines.lay -p ../../../tecplot/macros/export_eps.mcr

streamlines-metric.eps: metric.plt streamlines-metric.lay
	$(TECPLOT) -b -y $@ streamlines-metric.lay -p ../../../tecplot/macros/export_eps.mcr

vectors.eps: plot.nc vectors.lay
	$(TECPLOT) -b -y $@ vectors.lay -p ../../../tecplot/macros/export_eps.mcr

vectors-metric.eps: plot.nc vectors-metric.lay
	$(TECPLOT) -b -y $@ vectors-metric.lay -p ../../../tecplot/macros/export_eps.mcr


.PHONY : long-plot

long-1.0.eps long-1.5.eps long-2.0.eps long-3.0.eps	long-4.0.eps: long-plot
long-plot: long-plot.gp plot.nc
	gnuplot long-plot.gp

plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

clean:: 
	rm -f plot.nc 

metric.plt: plot.nc process.mcr
	$(TECPLOT) -b -p process.mcr

clean:: 
	rm -f metric.plt

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f batch.log
	rm -f hotstart_*.bin
	rm -f $(EPSFILES)
