# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February  1, 2002 by William A. Perkins
# Last Change: Fri Feb  1 07:54:43 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = \
	plot.1.eps \
	plot.2.eps 

MODELFILES = \
	mass2_v027.cfg \
	zero.dat

all: $(EPSFILES)

plot.1.eps: plot.gp gage.1.nc
	sed -e 's/gage\.nc/gage.1.nc/g' plot.gp | gnuplot > $@

plot.2.eps: plot.gp gage.2.nc
	sed -e 's/gage\.nc/gage.2.nc/g' plot.gp | gnuplot > $@

gage.1.nc plot.1.nc: $(MODELFILES) flow.dat bcspecs.dat.1 $(MODEL)
	cp bcspecs.dat.1 bcspecs.dat
	$(MODEL)
	mv gage.nc gage.1.nc
	mv plot.nc plot.1.nc

gage.2.nc plot.2.nc: $(MODELFILES) rate.dat bcspecs.dat.2 $(MODEL)
	cp bcspecs.dat.2 bcspecs.dat
	$(MODEL)
	mv gage.nc gage.2.nc
	mv plot.nc plot.2.nc

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc gage.nc  plot.1.nc gage.1.nc plot.2.nc gage.2.nc
	rm -f bcspecs.dat
	rm -f $(EPSFILES)
