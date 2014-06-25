# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 14, 2002 by William A. Perkins
# Last Change: 2014-06-25 13:00:50 d3g096
# -------------------------------------------------------------

MPIRUN = mpirun
NPROC = 4
MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES = \
	plot-conc.eps \
	plot-conc-metric.eps

MODELFILES = \
	bcspecs.dat \
	conc-bc.dat \
	flow.dat \
	gage_control.dat \
	grid.dat \
	mass2.cfg \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat

.PHONY: all 

all: $(EPSFILES)

plot000.cgns: $(MODELFILES) $(MODEL)
	$(MPIRUN) -np $(NPROC) $(MODEL)

probe.dat: plot000.cgns plot.lay probe.mcr
	$(TECPLOT) -b -p probe.mcr plot.lay

plot-conc.eps: plot-conc.gp probe.dat

plot-conc-metric.eps: plot-conc-metric.gp probe.dat

%.eps: %.gp
	gnuplot $< > $@

clean:: 
	rm -f *~
	rm -f error-warning*.out mass_source_monitor.out output*.out status*.out gage*.out
	rm -f gridplot1.dat 
	rm -f plot.nc gage.nc grid.cgns plot*.cgns
	rm -f stuff_balance.out probe.dat 
	rm -f tecplot.phy batch.log
	rm -f $(EPSFILES)
