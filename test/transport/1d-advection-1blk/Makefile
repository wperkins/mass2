# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Wed Nov  5 07:11:09 2008 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2
TECPLOT = tecplot

DSCHEMES = UPWIND CDS SOU MSOU MUSCL
TSCHEMES = 0.0 1.0
COURANTS = 1.0 6.0 0.1

PLOTEPSFILES = \
    $(foreach t, $(TSCHEMES), \
	$(foreach d, $(DSCHEMES), \
	    $(foreach c, $(COURANTS), plot-Cn-$(c)-$(d)-$(t).eps)))

EPSFILES = bcplot.eps $(PLOTEPSFILES)

MODELFILES = \
	bcspecs.dat \
	conc-bc.dat \
	flow.dat \
	gage_control.dat \
	grid.dat \
	mass2_v027.base \
	scalar_bcspecs.dat \
	scalar_source.base \
	stage.dat

PLOTFILES = $(PLOTEPSFILES:%.eps=%.nc)

all: $(EPSFILES)

$(PLOTEPSFILES): plot-Cn-%.eps: plot-Cn-%.nc plot.gp
	sed -e 's/plot\.nc/'"plot-Cn-$*.nc"'/g' plot.gp | gnuplot > $@

$(PLOTFILES): plot-Cn-%.nc: $(MODELFILES) $(MODEL)
	c=`expr "$*" : '\([^-]*\)-[^-]*-[^-]*' `; \
	d=`expr "$*" : '[^-]*-\([^-]*\)-[^-]*' `; \
	t=`expr "$*" : '[^-]*-[^-]*-\([^-]*\)' `; \
	dt=`echo '20*'"$$c" | bc `; \
	os=`echo '36/'"$$c" | bc `; \
	sed -e "s/@DT@/$$dt/" -e "s/@N@/$$os/g" -e "s/@TS@/$$t/g" mass2_v027.base > mass2_v027.cfg; \
	sed -e "s/@SCHEME@/$$d/g" scalar_source.base > scalar_source.dat
	$(MODEL)
	mv plot.nc $@

bcplot.eps: bcplot.gp conc-bc.dat
	gnuplot bcplot.gp > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out stuff_balance.out
	rm -f gridplot1.dat mass2_v027.cfg
	rm -f plot.nc gage.nc $(PLOTFILES)
	rm -f batch.log
	rm -f $(EPSFILES)
