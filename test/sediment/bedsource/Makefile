# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 19, 2002 by William A. Perkins
# Last Change: Tue Apr  8 13:38:16 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2_v027

EPSFILES = \
	plot-mass-nobed.eps \
	plot-mass-bed.eps

MODELFILES =									\
	bcspecs.dat									\
	flow.dat									\
	grid.dat									\
	mass2_v027.base								\
	scalar_bcspecs.dat							\
	scalar_source.dat							\
	silt-bc.dat									\
	stage.dat									\
	stuff-bc.dat								\
	stuff-list.dat								\
	stuff-map-1.dat								\
	stuff-map.dat								\
	stuff-mass-14.dat							\
	stuff-silt-bc.dat

all: $(EPSFILES)

clean:: 
	rm -f $(EPSFILES)

# The first simulation has no bed

plot.nobed.nc: $(MODELFILES) $(MODEL)
	sed -e 's/0\.25/0.00/' mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot.nobed.nc

clean:: 
	rm -f plot.nobed.nc

# The second simulation has a bed

plot.bed.nc: $(MODELFILES) $(MODEL)
	cp mass2_v027.base mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot.bed.nc

clean:: 
	rm -f plot.bed.nc

clean::
	rm -f plot.nc gage.nc
	rm -f mass2_v027.cfg

# pictures

plot-mass-nobed.eps: plot-mass.gp plot.nobed.nc
	sed -e 's/plot\.nc/plot.nobed.nc/g' plot-mass.gp | gnuplot > $@

plot-mass-bed.eps: plot-mass.gp plot.bed.nc
	sed -e 's/plot\.nc/plot.bed.nc/g' plot-mass.gp | gnuplot > $@

clean::
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
