# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February  1, 2002 by William A. Perkins
# Last Change: Thu Aug 20 09:00:27 2009 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2
TECPLOT = tecplot
GNUPLOT = gnuplot

EPSFILES = \
	depth.1992.eps \
	temp.1992.eps \
	temp.summer.1992.eps \
	evap.summer.1992.eps

MODELFILES = \
	mass2_v027.cfg \
	bcspecs.dat \
	scalar_bcspecs.dat \
	scalar_source.dat \
	pond.dat \
	met.dat \
	recharge.dat \
	recharge_temp.dat

all: $(EPSFILES)

$(EPSFILES): %.eps: %.gp gage_1.out
	$(GNUPLOT) $*.gp > $*.eps


gage_1.out: $(MODELFILES) $(MODEL)
	time $(MODEL)
	touch gage_1.out

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gage_*.out
	rm -f plot*.cgns
	rm -f gridplot1.dat
	rm -f plot.nc gage.nc
	rm -f $(EPSFILES)
