# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 19, 2002 by William A. Perkins
# Last Change: Fri May 16 06:42:19 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2

EPSFILES = \
	plot-mass.eps

MODELFILES =									\
	bcspecs.dat									\
	flow.dat									\
	grid.dat									\
	mass2_v027.cfg								\
	scalar_bcspecs.dat							\
	scalar_source.dat							\
	silt-bc.dat									\
	stage.dat									\
	stuff-bc.dat								\
	stuff-list.dat								\
	stuff-map-1.dat								\
	stuff-map.dat								\
	stuff-mass-14.dat							\
	stuff-silt-bc.dat \
	bed-flow-list.dat \
	bed-flow-mass-14.dat

all: $(EPSFILES)

clean:: 
	rm -f $(EPSFILES)

plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

clean::
	rm -f plot.nc gage.nc

# pictures

plot-mass.eps: plot-mass.gp plot.nc
	gnuplot plot-mass.gp > $@

clean::
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
