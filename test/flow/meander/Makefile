# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 20, 2004 by William A. Perkins
# Last Change: Fri Apr  2 09:28:10 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL=../../../mass2
TECPLOT=tecplot9


EPSFILES= \
	plot-u-a.eps \
	plot-u-b.eps \
	plot-u-c.eps \
	plot-u-d.eps \
	vectors-slip.eps \
	vectors-rough.eps 


MODELFILES =									\
	bcspecs.dat									\
	flow.dat									\
	meander1.dat								\
	mass2_v027.base								\
	stage.dat									\
	gage_control.dat							\
	scalar_bcspecs.dat							\
	scalar_source.dat							\
	stuff-list.dat								\
	lstuff-map-list.dat							\
	rstuff-map-list.dat							\
	cstuff-map-list.dat

all: $(EPSFILES)

plot-u-a.eps plot-u-b.eps plot-u-c.eps plot-u-d.eps: plot-u.gp plot.slip.nc plot.rough.nc
	gnuplot plot-u.gp


vectors-slip.eps: plot.slip.nc vectors.lay
	sed -e s/plot.nc/plot.slip.nc/ vectors.lay > tmp.lay
	$(TECPLOT) -b -y $@ tmp.lay -p ../../../tecplot/macros/export_eps.mcr
	rm tmp.lay

plot.slip.nc: $(MODELFILES) $(MODEL)
	cp mass2_v027.base mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot.slip.nc

vectors-rough.eps: plot.rough.nc vectors.lay
	sed -e s/plot.nc/plot.rough.nc/ vectors.lay > tmp.lay
	$(TECPLOT) -b -y $@ tmp.lay -p ../../../tecplot/macros/export_eps.mcr
	rm tmp.lay


plot.rough.nc: $(MODELFILES) $(MODEL) roughness_coeff.dat
	sed -e 25s/0.01125/0.00925/ -e 25s/F/T/ mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot.rough.nc

clean::
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc
	rm -f batch.log
	rm -f $(EPSFILES)
