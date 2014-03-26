# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Mon Jan 17 06:46:52 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
# $Id$

MPIRUN = mpirun
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

LOCS = 1.0 1.5 2.0 3.0 4.0
LONGPLOTS = $(foreach l, $(LOCS), $(foreach c, $(CASES), long-$(l)-$(c).eps))
LONGDATA = $(foreach l, $(LOCS), $(foreach c, $(CASES), long-$(l)-$(c).dat))


# EPSFILES = $(STREAMEPSFILES) $(VECTEPSFILES) $(METRICSTREAMEPSFILES) $(METRICVECTEPSFILES) $(LONGPLOTS)
EPSFILES = $(METRICSTREAMEPSFILES) $(METRICVECTEPSFILES) $(LONGPLOTS)

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	mass2_v027.base \
	stage.dat \
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

LONGTARGETS = $(foreach c, $(CASES), long-plot-$(c))
.PHONY : $(LONGTARGETS)

$(LONGTARGETS): long-plot-%: long-plot.gp $(foreach l, $(LOCS), long-${l}-%.dat)
	sed -e "s/@CASE@/$*/" long-plot.gp | gnuplot

$(filter long-1.0-%.eps, $(LONGPLOTS)): long-1.0-%.eps: long-plot-%
$(filter long-1.5-%.eps, $(LONGPLOTS)): long-1.5-%.eps: long-plot-%
$(filter long-2.0-%.eps, $(LONGPLOTS)): long-2.0-%.eps: long-plot-%
$(filter long-3.0-%.eps, $(LONGPLOTS)): long-3.0-%.eps: long-plot-%
$(filter long-4.0-%.eps, $(LONGPLOTS)): long-4.0-%.eps: long-plot-%

$(filter long-1.0-%.dat, $(LONGPLOTS)): long-1.0-%.dat: plot-%.cgns
$(filter long-1.5-%.dat, $(LONGPLOTS)): long-1.5-%.dat: plot-%.cgns
$(filter long-2.0-%.dat, $(LONGPLOTS)): long-2.0-%.dat: plot-%.cgns
$(filter long-3.0-%.dat, $(LONGPLOTS)): long-3.0-%.dat: plot-%.cgns
$(filter long-4.0-%.dat, $(LONGPLOTS)): long-4.0-%.dat: plot-%.cgns

long-%.dat: probe.lay probe.mcr
	y=`expr "$*" : '\([^-][^-]*\)-.*' `; \
	y=`echo $$y | awk '{print 0.5*($$1+0.0)}'`; \
	c=`expr "$*" : '[^-][^-]*-\(.*\)' `; \
	echo "$$y, $$c"; \
	sed -e "s/UPWIND/$$c/g" probe.lay > junk.lay; \
	sed -e "s/@Y@/$$y/g" -e "s/probe.dat/$@/g" probe.mcr > junk.mcr
	$(TECPLOT) -b -p junk.mcr junk.lay

clean:: 
	rm -f *~
	rm -f error-warning*.out mass_source_monitor.out output*.out status*.out
	rm -f gridplot1.dat
	rm -f plot.nc grid.cgns plot000.cgns $(PLOTFILES)
	rm -f $(LONGDATA)
	rm -f batch.log junk.lay mass2_v027.cfg mass2.cfg hotstart*.bin
	rm -f $(EPSFILES)

