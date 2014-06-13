# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: 2014-06-12 12:47:35 d3g096
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

EPSFILES = $(PLOTEPSFILES)

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

PLOTFILES = $(PLOTEPSFILES:%.eps=%.cgns)

all: $(EPSFILES)

$(PLOTEPSFILES): plot-Cn-%.eps: plot-Cn-%.cgns plot.gp
	c=`expr "$*" : '\([^-]*\)-[^-]*-[^-]*' `; \
	sed -e 's/plot\.nc/'"plot-Cn-$*.nc"'/g' \
	    -e 's/TheTitle/Courant Number = '"$$c"'/' plot.gp | gnuplot > $@

$(PLOTFILES): plot-Cn-%.cgns: $(MODELFILES) $(MODEL)
	c=`expr "$*" : '\([^-]*\)-[^-]*-[^-]*' `; \
	d=`expr "$*" : '[^-]*-\([^-]*\)-[^-]*' `; \
	t=`expr "$*" : '[^-]*-[^-]*-\([^-]*\)' `; \
	dt=`echo '20*'"$$c" | bc `; \
	os=`echo '18/'"$$c" | bc `; \
	sed -e "s/@DT@/$$dt/" -e "s/@N@/$$os/g" -e "s/@TS@/$$t/g" mass2_v027.base > mass2.cfg; \
	sed -e "s/@SCHEME@/$$d/g" scalar_source.base > scalar_source.dat
	$(MODEL)
	mv plot000.cgns $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat mass2.cfg mass2_v027.cfg scalar_source.dat
	rm -f plot.nc gage.nc $(PLOTFILES) plot000.cgns grid.cgns
	rm -f batch.log
	rm -f $(EPSFILES)
