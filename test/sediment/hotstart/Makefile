# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 19, 2002 by William A. Perkins
# Last Change: Tue Mar 19 11:01:31 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2_v027

EPSFILES = \
	plot.eps

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat \
	stuff-bc.dat \
	stuff-silt-bc.dat

all: $(EPSFILES)

clean:: 
	rm -f $(EPSFILES)

# hydro only warmup

hotstart_04-01-1996_030000.bin: mass2_v027.warmup $(MODELFILES) $(MODEL)
	cp mass2_v027.warmup mass2_v027.cfg
	$(MODEL)

clean:: 
	rm -f hotstart_04-01-1996_030000.bin

# deposition

plot.depos.nc hotstart_04-01-1996_180000.bin: mass2_v027.depos \
		$(MODELFILES) $(MODEL)	hotstart_04-01-1996_030000.bin
	cp mass2_v027.depos mass2_v027.cfg
	cp hotstart_04-01-1996_030000.bin hotstart.bin
	$(MODEL)
	mv plot.nc $@

clean:: 
	rm -f plot.depos.nc
	rm -f hotstart_04-01-1996_180000.bin

# erosion

plot.erode.nc: mass2_v027.erode \
		$(MODELFILES) $(MODEL) hotstart_04-01-1996_180000.bin
	cp mass2_v027.erode mass2_v027.cfg
	cp hotstart_04-01-1996_180000.bin hotstart.bin
	$(MODEL)
	mv plot.nc $@

clean:: 
	rm -f plot.erode.nc

clean::
	rm -f plot.nc gage.nc
	rm -f mass2_v027.cfg

# pictures

plot.eps: plot.gp plot.depos.nc plot.erode.nc

%.eps: %.gp
	gnuplot $< > $@

clean::
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f hotstart*.bin
	rm -f gage_control.dat
	rm -f gridplot1.dat
