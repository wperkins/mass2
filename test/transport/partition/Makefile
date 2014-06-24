# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 14, 2002 by William A. Perkins
# Last Change: 2014-06-24 09:08:11 d3g096
# -------------------------------------------------------------

MPIRUN = mpirun
NPROC = 4
MODEL = ../../../mass2
TECPLOT = tecplot
GNUPLOT = gnuplot

EPSFILES = \
	plot-conc-nodecay.eps \
	plot-conc-decay.eps \

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	mass2.cfg \
	scalar_bcspecs.dat \
	scalar_source.base \
	sediment-bc.dat \
	stage.dat \
	stuff-bc.dat \
	zero.dat

.PHONY: all

all: $(EPSFILES)

plot-nodecay.cgns: $(MODELFILES) $(MODEL)
	cp scalar_source.base scalar_source.dat
	$(MPIRUN) -np $(NPROC) $(MODEL)
	mv plot000.cgns plot-nodecay.cgns


plot-decay.cgns: $(MODELFILES) $(MODEL)
	sed -e '9s/0.0/1.584E-04/' scalar_source.base > scalar_source.dat
	$(MPIRUN) -np $(NPROC) $(MODEL)
	mv plot000.cgns plot-decay.cgns

probe-nodecay.dat: plot-nodecay.cgns plot.lay probe.mcr
	sed -e "s/@CGNS@/plot-nodecay.cgns/" plot.lay > junk.lay
	$(TECPLOT) -b -p probe.mcr junk.lay
	mv probe.dat $@

probe-decay.dat: plot-decay.cgns plot.lay probe.mcr
	sed -e "s/@CGNS@/plot-decay.cgns/" plot.lay > junk.lay
	$(TECPLOT) -b -p probe.mcr junk.lay
	mv probe.dat $@

plot-conc-nodecay.eps: probe-nodecay.dat plot-conc.gp
	sed -e "s/@probe@/probe-nodecay.dat/" plot-conc.gp | \
		$(GNUPLOT) > $@

plot-conc-decay.eps: probe-decay.dat plot-conc.gp
	sed -e "s/@probe@/probe-decay.dat/" plot-conc.gp | \
		$(GNUPLOT) > $@


clean:: 
	rm -f *~
	rm -f error-warning*.out mass_source_monitor.out output*.out status*.out
	rm -f gridplot1.dat 
	rm -f plot.nc gage.nc plot-nodecay.nc plot-decay.nc 
	rm -f grid.cgns plot000.cgns plot-nodecay.cgns plot-decay.cgns
	rm -f junk.lay batch.log probe.dat tecplot.phy probe-nodecay.dat probe-decay.dat probe.dat
	rm -f *_balance.out scalar_source.dat
	rm -f $(EPSFILES)
