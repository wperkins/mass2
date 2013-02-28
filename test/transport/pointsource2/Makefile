# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Thu Feb  1 19:06:08 2007 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id: Makefile 618 2007-02-02 16:05:36Z perk $

MODEL = ../../../mass2
TECPLOT = tecplot
CGNSVERSION = cgnsversion

EPSFILES = \
	plan.eps

MODELFILES = \
	background.dat \
	bcspecs.dat \
	flow.dat \
	srcflow.dat \
	srcconc.dat \
	gage_control.dat \
	grid.dat \
	mass2_v027.cfg \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat



all: $(EPSFILES)

plot.cgns: $(MODELFILES) $(MODEL)
	$(MODEL)
	$(CGNSVERSION) -r -k 2.5 plot000.cgns $@

plan.eps: plan.lay plot.cgns
	$(TECPLOT) -b -y $@ plan.lay -p ../../../tecplot/macros/export_eps.mcr

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc gage.nc plot000.cgns grid.cgns plot.cgns
	rm -f gage*.out
	rm -f batch.log
	rm -f $(EPSFILES)
