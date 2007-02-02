# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Wed Mar  3 14:59:19 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2

EPSFILES = \
	plot-depth-flow-stage.eps \
	plot-elev-flow-stage.eps \
	plot-vel-flow-stage.eps \
	plot-depth-flow-stage-metric.eps \
	plot-elev-flow-stage-metric.eps \
	plot-vel-flow-stage-metric.eps \
	plot-depth-flow-flow.eps \
	plot-elev-flow-flow.eps \
	plot-vel-flow-flow.eps \
	plot-depth-flow-flow-metric.eps \
	plot-elev-flow-flow-metric.eps \
	plot-vel-flow-flow-metric.eps
# 	plot-depth-stage-stage.eps \
# 	plot-elev-stage-stage.eps \
# 	plot-vel-stage-stage.eps \
# 	plot-depth-stage-stage-metric.eps \
# 	plot-elev-stage-stage-metric.eps \
# 	plot-vel-stage-stage-metric.eps \

MODELFILES = \
	bcspecs.base \
	mass2_v027.base \

all: $(EPSFILES)

$(filter %-flow-stage.eps, $(EPSFILES)): plot-flow-stage.nc
$(filter %-stage-stage.eps, $(EPSFILES)): plot-stage-stage.nc
$(filter %-flow-flow.eps, $(EPSFILES)): plot-flow-flow.nc
$(filter %-stage-flow.eps, $(EPSFILES)): plot-stage-flow.nc
$(filter %-flow-stage-metric.eps, $(EPSFILES)): plot-flow-stage.nc
$(filter %-stage-stage-metric.eps, $(EPSFILES)): plot-stage-stage.nc
$(filter %-flow-flow-metric.eps, $(EPSFILES)): plot-flow-flow.nc
$(filter %-stage-flow-metric.eps, $(EPSFILES)): plot-stage-flow.nc


plot-flow-stage.nc hotstart.bin: $(MODELFILES) $(MODEL) flow.dat stage.dat
	cp bcspecs.base bcspecs.dat
	cp mass2_v027.base mass2_v027.cfg
	$(MODEL)
	mv plot.nc $@
	mv hotstart_04-01-1996_060000.bin hotstart.bin

clean:: 
	rm -f plot-flow-stage.nc
	rm -f bcspecs.dat mass2_v027.cfg
	rm -f hotstart.bin

plot-stage-stage.nc: $(MODELFILES) $(MODEL) stage.dat ustage.dat
	sed -e 's/FLUX/ELEV/g' -e 's/flow\.dat/ustage.dat/g' bcspecs.base > bcspecs.dat
	cp mass2_v027.base mass2_v027.cfg
	$(MODEL)
	mv plot.nc $@
	rm hotstart_04-01-1996_060000.bin

clean:: 
	rm -f plot-stage-stage.nc
	rm -f bcspecs.dat mass2_v027.cfg
	rm -f hotstart.bin

plot-stage-flow.nc: $(MODELFILES) $(MODEL) flow.dat ustage.dat
	sed -e '1s/FLUX/ELEV/g' -e '1s/flow\.dat/ustage.dat/g' \
		-e '2s/ELEV/FLUX/g' -e '2s/stage\.dat/flow.dat/g' bcspecs.base > bcspecs.dat
	cp mass2_v027.base mass2_v027.cfg
	$(MODEL)
	mv plot.nc $@

clean:: 
	rm -f plot-stage-flow.nc
	rm -f bcspecs.dat mass2_v027.cfg
	rm -f hotstart.bin

plot-flow-flow.nc: $(MODELFILES) $(MODEL) hotstart.bin flow.dat
	sed -e 's/ELEV/FLUX/g' -e 's/stage\.dat/flow.dat/g' bcspecs.base > bcspecs.dat
	sed -e '11s/^T/F/' -e '12s/^F/T/' mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc $@

clean:: 
	rm -f plot-flow-flow.nc
	rm -f bcspecs.dat mass2_v027.cfg


%-stage-flow.eps: %.gp
	sed -e 's/plot\.nc/plot-stage-flow.nc/g' $< | gnuplot > $@

%-flow-stage.eps: %.gp
	sed -e 's/plot\.nc/plot-flow-stage.nc/g' $< | gnuplot > $@

%-stage-stage.eps: %.gp
	sed -e 's/plot\.nc/plot-stage-stage.nc/g' $< | gnuplot > $@

%-flow-flow.eps: %.gp
	sed -e 's/plot\.nc/plot-flow-flow.nc/g' $< | gnuplot > $@

%-stage-flow-metric.eps: %-metric.gp
	sed -e 's/plot\.nc/plot-stage-flow.nc/g' $< | gnuplot > $@

%-flow-stage-metric.eps: %-metric.gp
	sed -e 's/plot\.nc/plot-flow-stage.nc/g' $< | gnuplot > $@

%-stage-stage-metric.eps: %-metric.gp
	sed -e 's/plot\.nc/plot-stage-stage.nc/g' $< | gnuplot > $@

%-flow-flow-metric.eps: %-metric.gp
	sed -e 's/plot\.nc/plot-flow-flow.nc/g' $< | gnuplot > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc 
	rm -f $(EPSFILES)