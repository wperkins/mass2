# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 19, 2002 by William A. Perkins
# Last Change: Thu May 15 11:02:38 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2_v027

EPSFILES = \
	plot-mass-1.eps \
	plot-mass-2.eps \
	plot-mass-3.eps

MODELFILES =									\
	bcspecs.dat									\
	flow.dat									\
	gage_control.dat \
	grid.dat									\
	mass2_v027.cfg								\
	scalar_bcspecs.dat							\
	scalar_source.base							\
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

# Case 1: no decay

plot.1.nc stuff_balance.1.out silt_balance.1.out: $(MODELFILES) $(MODEL)
	sed -e "s/@HL@/0.0/g" scalar_source.base > scalar_source.dat
	$(MODEL)
	mv plot.nc plot.1.nc
	mv stuff_balance.out stuff_balance.1.out
	mv silt_balance.out silt_balance.1.out

clean:: 
	rm -f plot.1.nc scalar_source.dat stuff_balance.1.out silt_balance.1.out

# Case 2: 24-hour half-life

plot.2.nc stuff_balance.2.out silt_balance.2.out: $(MODELFILES) $(MODEL)
	sed -e "s/@HL@/2.7379E-03/g" scalar_source.base > scalar_source.dat
	$(MODEL)
	mv plot.nc plot.2.nc
	mv stuff_balance.out stuff_balance.2.out
	mv silt_balance.out silt_balance.2.out

clean:: 
	rm -f plot.2.nc scalar_source.dat stuff_balance.2.out silt_balance.2.out

# Case 3: 12-hour half-life

plot.3.nc stuff_balance.3.out silt_balance.3.out: $(MODELFILES) $(MODEL)
	sed -e "s/@HL@/1.3689E-03/g" scalar_source.base > scalar_source.dat
	$(MODEL)
	mv plot.nc plot.3.nc
	mv stuff_balance.out stuff_balance.3.out
	mv silt_balance.out silt_balance.3.out

clean:: 
	rm -f plot.3.nc scalar_source.dat stuff_balance.3.out silt_balance.3.out

plot-mass-1.eps: plot-mass.gp plot.1.nc
	sed -e 's/plot\.nc/plot.1.nc/g' plot-mass.gp | gnuplot > $@

plot-mass-2.eps: plot-mass.gp plot.2.nc
	sed -e 's/plot\.nc/plot.2.nc/g' plot-mass.gp | gnuplot > $@

plot-mass-3.eps: plot-mass.gp plot.3.nc
	sed -e 's/plot\.nc/plot.3.nc/g' plot-mass.gp | gnuplot > $@

clean::
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
