# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 18, 2002 by William A. Perkins
# Last Change: Mon Mar 25 07:18:21 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2
TECPLOT = tecplot

cases := a b c
gages := 1 2 3 4 6 9 16 22
EPSFILES = $(foreach g, $(gages), $(foreach c, $(cases), gage$(g).case$(c).eps))

MODELFILES = \
	bcspecs.base \
	grid-feet.dat \
	mass2_v027.cfg \
	gage_control.dat \
	zero.dat

all: $(EPSFILES)

plot.casea.nc gage.casea.nc: casea-velocity.dat $(MODELFILES) $(MODEL)
	sed -e 's/case/casea/g' bcspecs.base > bcspecs.dat
	$(MODEL)
	mv gage.nc gage.casea.nc
	mv plot.nc plot.casea.nc 

clean::
	rm -f gage.casea.nc plot.casea.nc 

plot.caseb.nc gage.caseb.nc: caseb-velocity.dat $(MODELFILES) $(MODEL)
	sed -e 's/case/caseb/g' bcspecs.base > bcspecs.dat
	$(MODEL)
	mv gage.nc gage.caseb.nc
	mv plot.nc plot.caseb.nc 

clean::
	rm -f gage.caseb.nc plot.caseb.nc 

plot.casec.nc gage.casec.nc: casec-velocity.dat $(MODELFILES) $(MODEL)
	sed -e 's/case/casec/g' bcspecs.base > bcspecs.dat
	$(MODEL)
	mv gage.nc gage.casec.nc
	mv plot.nc plot.casec.nc 

clean::
	rm -f gage.casec.nc plot.casec.nc 

$(filter %.casea.eps, $(EPSFILES)): gage.casea.nc
$(filter %.caseb.eps, $(EPSFILES)): gage.caseb.nc
$(filter %.casec.eps, $(EPSFILES)): gage.casec.nc

$(EPSFILES): gage.gp gageplot.sh
	sh gageplot.sh $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat bcspecs.dat
	rm -f plot.nc gage.nc
	rm -f $(EPSFILES)
