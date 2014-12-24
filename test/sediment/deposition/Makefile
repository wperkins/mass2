# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 19, 2002 by William A. Perkins
# Last Change: Tue Apr  8 13:38:47 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2

EPSFILES = \
	plot-sed.eps \
	plot-conc.eps \
	plot-smass.eps \
	plot-mass.eps

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	mass2_v027.cfg \
	scalar_bcspecs.dat \
	scalar_source.dat \
	stage.dat \
	stuff-bc.dat \
	zero.dat

all: $(EPSFILES)

clean:: 
	rm -f $(EPSFILES)

plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

clean::
	rm -f plot.nc gage.nc

$(EPSFILES): plot.nc

%.eps: %.gp
	gnuplot $< > $@

clean::
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f hotstart*.bin
