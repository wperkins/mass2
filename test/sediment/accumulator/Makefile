# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 19, 2002 by William A. Perkins
# Last Change: Wed Mar 20 10:58:53 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2_v027

EPSFILES = \
	plot-mass.inst.eps \
	plot-mass.avg.eps

#	plot-sediment.eps \
#	plot-stuff.eps

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	gage_control.dat \
	grid.dat \
	scalar_bcspecs.dat \
	scalar_source.dat \
	silt-bc.dat \
	stage.dat \
	stuff-bc.dat \
	stuff-silt-bc.dat 

all: $(EPSFILES)

clean:: 
	rm -f $(EPSFILES)

# Run a warm up

hotstart.bin: mass2_v027.warmup $(MODELFILES) $(MODEL)
	cp mass2_v027.warmup mass2_v027.cfg
	$(MODEL)
	mv hotstart_04-01-1996_000000.bin hotstart.bin

clean:: 
	rm -f hotstart.bin

# Run a case w/o the accumulator

plot.inst.nc gage.inst.nc: mass2_v027.run hotstart.bin $(MODELFILES) $(MODEL)
	cp mass2_v027.run mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot.inst.nc
	mv gage.nc gage.inst.nc

clean:: 
	rm -f plot.inst.nc gage.inst.nc

# Run a case w/ the accumulator

plot.avg.nc gage.avg.nc: mass2_v027.run hotstart.bin $(MODELFILES) $(MODEL)
	sed -e '38s/F/T/' mass2_v027.run > mass2_v027.cfg
	$(MODEL)
	mv plot.nc plot.avg.nc
	mv gage.nc gage.avg.nc

clean:: 
	rm -f plot.avg.nc gage.avg.nc

# 

clean::
	rm -f mass2_v027.cfg
	rm -f plot.nc gage.nc


plot-mass.inst.eps: plot.inst.nc plot-mass.gp
	gnuplot <  plot-mass.gp > $@
plot-mass.avg.eps: plot.avg.nc plot-mass.gp
	sed -e 's/plot\.inst\.nc/plot.avg.nc/g' plot-mass.gp | gnuplot > $@
plot-sediment.eps: plot.inst.nc plot.avg.nc gage.avg.nc plot-sediment.gp
		gnuplot <  plot-sediment.gp  > $@
plot-stuff.eps: plot.inst.nc plot.avg.nc gage.avg.nc plot-stuff.gp
	gnuplot <  plot-stuff.gp > $@

clean::
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
