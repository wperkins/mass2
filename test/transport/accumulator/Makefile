# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 14, 2002 by William A. Perkins
# Last Change: Thu Feb 21 08:45:49 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = \
	plot-conc.eps

MODELFILES = \
	bcspecs.dat \
	conc-bc.dat \
	flow.dat \
	grid.dat \
	mass2_v027.base \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat

all: $(EPSFILES)

hotstart.bin: $(MODELFILES) $(MODEL)
	sed -e '15s/05:/03:/' -e '16s/10:/05:/'  mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv hotstart_01-01-1997_060000.bin $@

plot.inst.nc: $(MODELFILES) $(MODEL) hotstart.bin
	sed -e '15s/05:/06:/' -e '12s/F/T/' mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc $@

plot.avg.nc: $(MODELFILES) $(MODEL) hotstart.bin
	sed -e '15s/05:/06:/' -e '12s/F/T/' -e '38s/F/T/' mass2_v027.base > mass2_v027.cfg
	$(MODEL)
	mv plot.nc $@

plot-conc.eps: plot-conc.gp plot.inst.nc plot.avg.nc

%.eps: %.gp
	gnuplot $< > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat gage_control.dat mass2_v027.cfg
	rm -f plot.nc gage.nc  plot.inst.nc plot.avg.nc
	rm -f hotstart*.bin
	rm -f $(EPSFILES)
