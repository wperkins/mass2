# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Wed Nov  5 11:42:59 2008 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2
TECPLOT = tecplot

CASES = UPWIND CDS SOU MSOU MUSCL

PLOTFILES = $(foreach c, $(CASES), plot-$(c).nc)

STREAMEPSFILES = $(foreach c, $(CASES), streamlines-$(c).eps)
VECTEPSFILES = $(foreach c, $(CASES), vectors-$(c).eps)
METRICSTREAMEPSFILES = $(foreach c, $(CASES), streamlines-$(c)-metric.eps)
METRICVECTEPSFILES = $(foreach c, $(CASES), vectors-$(c)-metric.eps)

LOCS = 1.0 1.5 2.0 3.0 4.0
LONGPLOTS = $(foreach l, $(LOCS), $(foreach c, $(CASES), long-$(l)-$(c).eps))


EPSFILES = $(STREAMEPSFILES) $(VECTEPSFILES) $(METRICSTREAMEPSFILES) $(METRICVECTEPSFILES) $(LONGPLOTS)

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	mass2_v027.base \
	stage.dat \
	gage_control.dat

all: $(EPSFILES)

$(STREAMEPSFILES): streamlines-%.eps: plot-%.nc streamlines.lay
	sed -e "s/plot.nc/plot-$*.nc/g" streamlines.lay > junk.lay
	$(TECPLOT) -b -y $@ junk.lay -p ../../../tecplot/macros/export_eps.mcr

$(VECTEPSFILES): vectors-%.eps: plot-%.nc vectors.lay
	sed -e "s/plot.nc/plot-$*.nc/g" vectors.lay > junk.lay
	$(TECPLOT) -b -y $@ junk.lay -p ../../../tecplot/macros/export_eps.mcr

$(METRICSTREAMEPSFILES): streamlines-%-metric.eps: plot-%.nc streamlines-metric.lay
	sed -e "s/plot.nc/plot-$*.nc/g" streamlines-metric.lay > junk.lay
	$(TECPLOT) -b -y $@ junk.lay -p ../../../tecplot/macros/export_eps.mcr

$(METRICVECTEPSFILES): vectors-%-metric.eps: plot-%.nc vectors-metric.lay
	sed -e "s/plot.nc/plot-$*.nc/g" vectors-metric.lay > junk.lay
	$(TECPLOT) -b -y $@ junk.lay -p ../../../tecplot/macros/export_eps.mcr

$(PLOTFILES): plot-%.nc: $(MODELFILES) $(MODEL)
	sed -e "s/@S@/$*/g" mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc $@

LONGTARGETS = $(foreach c, $(CASES), long-plot-$(c))
.PHONY : $(LONGTARGETS)

$(LONGTARGETS): long-plot-%: plot-%.nc long-plot.gp
	sed -e "s/@CASE@/$*/" long-plot.gp | gnuplot

$(filter long-1.0-%.eps, $(LONGPLOTS)): long-1.0-%.eps: long-plot-%
$(filter long-1.5-%.eps, $(LONGPLOTS)): long-1.5-%.eps: long-plot-%
$(filter long-2.0-%.eps, $(LONGPLOTS)): long-2.0-%.eps: long-plot-%
$(filter long-3.0-%.eps, $(LONGPLOTS)): long-3.0-%.eps: long-plot-%
$(filter long-4.0-%.eps, $(LONGPLOTS)): long-4.0-%.eps: long-plot-%

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc $(PLOTFILES)
	rm -f batch.log junk.lay mass2_v027.cfg hotstart*.bin
	rm -f $(EPSFILES)

