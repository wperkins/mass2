# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Wed Oct  9 10:04:00 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2_v027

EPSFILES = \
	plot-depth-stage.eps \
	plot-elev-stage.eps \
	plot-vel-stage.eps \
	plot-depth-usstage.eps \
	plot-elev-usstage.eps \
	plot-vel-usstage.eps \
	plot-depth-flow.eps \
	plot-elev-flow.eps \
	plot-vel-flow.eps

MODELFILES = \
	bcspecs.base \
	mass2_v027.base \

all: $(EPSFILES)

$(filter %-stage.eps, $(EPSFILES)): plot-stage.nc
$(filter %-usstage.eps, $(EPSFILES)): plot-usstage.nc
$(filter %-flow.eps, $(EPSFILES)): plot-flow.nc


plot-stage.nc hotstart.bin: $(MODELFILES) $(MODEL) flow.dat stage.dat
	cp bcspecs.base bcspecs.dat
	cp mass2_v027.base mass2_v027.cfg
	$(MODEL)
	mv plot.nc $@
	mv hotstart_04-01-1996_060000.bin hotstart.bin

clean:: 
	rm -f plot-stage.nc
	rm -f bcspecs.dat mass2_v027.cfg
	rm -f hotstart.bin

plot-usstage.nc: $(MODELFILES) $(MODEL) stage.dat ustage.dat
	sed -e 's/FLUX/ELEV/g' -e 's/flow\.dat/ustage.dat/g' bcspecs.base > bcspecs.dat
	cp mass2_v027.base mass2_v027.cfg
	$(MODEL)
	mv plot.nc $@
	rm hotstart_04-01-1996_060000.bin

clean:: 
	rm -f plot-usstage.nc
	rm -f bcspecs.dat mass2_v027.cfg
	rm -f hotstart.bin

plot-flow.nc: $(MODELFILES) $(MODEL) hotstart.bin flow.dat
	sed -e 's/ELEV/FLUX/g' -e 's/stage\.dat/flow.dat/g' bcspecs.base > bcspecs.dat
	sed -e '11s/^T/F/' -e '12s/^F/T/' mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc $@

clean:: 
	rm -f plot-flow.nc
	rm -f bcspecs.dat mass2_v027.cfg

%-stage.eps: %.gp
	sed -e 's/plot\.nc/plot-stage.nc/g' $< | gnuplot > $@

%-usstage.eps: %.gp
	sed -e 's/plot\.nc/plot-usstage.nc/g' $< | gnuplot > $@

%-flow.eps: %.gp
	sed -e 's/plot\.nc/plot-flow.nc/g' $< | gnuplot > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat gage_control.dat
	rm -f plot.nc 
	rm -f $(EPSFILES)