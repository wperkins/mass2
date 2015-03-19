# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 23, 2004 by William A. Perkins
# Last Change: 2015-01-06 08:51:52 d3g096
# -------------------------------------------------------------
# $Id$


MPIRUN = mpiexec
NPROC = 4
MODEL = ../../../mass2
TECPLOT = tecplot
CGNSVERSION = cgnsversion

CASES = UPWIND CDS SOU MSOU MUSCL

PLOTFILES = $(foreach c, $(CASES), plot-$(c).cgns)

STREAMEPSFILES = $(foreach c, $(CASES), streamlines-$(c).eps)
VECTEPSFILES = $(foreach c, $(CASES), vectors-$(c).eps)
METRICSTREAMEPSFILES = $(foreach c, $(CASES), streamlines-$(c)-metric.eps)
METRICVECTEPSFILES = $(foreach c, $(CASES), vectors-$(c)-metric.eps)

EPSFILES = $(METRICSTREAMEPSFILES) $(METRICVECTEPSFILES) 

MODELFILES =					\
	bcspecs.dat				\
	main_flow.dat				\
	jet_flow.dat				\
	grid.dat				\
	mass2_v027.base				\
	stage.dat				\
	gage_control.dat

all: $(EPSFILES)

$(STREAMEPSFILES): streamlines-%.eps: plot-%.cgns streamlines.lay
	sed -e "s/plot.cgns/plot-$*.cgns/g" streamlines.lay > junk.lay
	$(TECPLOT) -b -y $@ junk.lay -p ../../../tecplot/macros/export_eps.mcr

$(VECTEPSFILES): vectors-%.eps: plot-%.cgns vectors.lay
	sed -e "s/plot.cgns/plot-$*.cgns/g" vectors.lay > junk.lay
	$(TECPLOT) -b -y $@ junk.lay -p ../../../tecplot/macros/export_eps.mcr

$(METRICSTREAMEPSFILES): streamlines-%-metric.eps: plot-%.cgns streamlines-metric.lay
	sed -e "s/plot.cgns/plot-$*.cgns/g" streamlines-metric.lay > junk.lay
	$(TECPLOT) -b -y $@ junk.lay -p ../../../tecplot/macros/export_eps.mcr

$(METRICVECTEPSFILES): vectors-%-metric.eps: plot-%.cgns vectors-metric.lay
	sed -e "s/plot.cgns/plot-$*.cgns/g" vectors-metric.lay > junk.lay
	$(TECPLOT) -b -y $@ junk.lay -p ../../../tecplot/macros/export_eps.mcr

$(PLOTFILES): plot-%.cgns: $(MODELFILES) $(MODEL)
	sed -e "s/@S@/$*/g" mass2_v027.base > mass2.cfg
	$(MPIRUN) -np $(NPROC) $(MODEL)
	$(CGNSVERSION) -r -k 2.5 plot000.cgns $@

clean:: 
	rm -f *~ 
	rm -f error-warning*.out mass_source_monitor.out output*.out status*.out
	rm -f gridplot1.dat
	rm -f plot.nc grid.cgns plot000.cgns $(PLOTFILES)
	rm -f batch.log junk.lay mass2.cfg mass2_v027.cfg
	rm -f $(EPSFILES)
