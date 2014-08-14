# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: 2014-08-14 07:10:14 d3g096
# -------------------------------------------------------------
# $Id$

MPIRUN = mpirun
NPROC = 4
MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES = \
	plot-lat.eps \
	plot-long.eps \
	plan.eps \
	plan-metric.eps

MODELFILES = \
	background.dat \
	bcspecs.dat \
	flow.dat \
	gage_control.dat \
	grid.dat \
	mass2.cfg \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat \
	stuff-flux.dat


.PHONY: all 

all: $(EPSFILES)

plot000.cgns: $(MODELFILES) $(MODEL)
	$(MPIRUN) -np $(NPROC) $(MODEL)

.PHONE: probe

probe1x.dat probe2x.dat probe3x.dat probe4x.dat probe1y.dat probe2y.dat probe3y.dat: probe

probe: plot000.cgns plan.lay probe.mcr
	$(TECPLOT) -b -p probe.mcr plan.lay

plot-lat.eps: plot-lat.gp solution.pl plot000.cgns probe1x.dat probe2x.dat probe3x.dat probe4x.dat
plot-long.eps: plot-long.gp solution.pl plot000.cgns probe1y.dat probe2y.dat probe3y.dat
plot-mass.eps: plot-mass.gp plot000.cgns 

plan.eps: plan.lay plot000.cgns
	$(TECPLOT) -b -y $@ plan.lay -p ../../../tecplot/macros/export_eps.mcr

plan-metric.eps: plan.lay plot000.cgns ../mixing/metric.mcr
	$(TECPLOT) -b -y $@ plan.lay -p ../mixing/metric.mcr


%.eps: %.gp
	gnuplot $< > $@

clean:: 
	rm -f *~
	rm -f plot000.cgns grid.cgns
	rm -f error-warning*.out mass_source_monitor.out output*.out status*.out gage*.out
	rm -f gridplot1.dat
	rm -f plot.nc gage.nc
	rm -f batch.log
	rm -f $(EPSFILES)
