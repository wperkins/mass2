# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February  1, 2002 by William A. Perkins
# Last Change: 2013-11-20 12:50:16 d3g096
# -------------------------------------------------------------

MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES = \
	plot.eps

MODELFILES = \
	mass2_v027.cfg \
	zero.dat

all: $(EPSFILES)

plot.eps: plot.gp gage_A.out
	cat plot.gp | gnuplot > $@

gage_A.out: $(MODELFILES) cumulative_precip.dat bcspecs.dat $(MODEL)
	$(MODEL)

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc gage*.out
	rm -f $(EPSFILES)
