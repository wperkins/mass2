# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 13, 2002 by William A. Perkins
# Last Change: Mon Jan 17 07:09:36 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = \
	plot.case1.eps \
	plot.case2.eps \
	plot.case3.eps

MODELFILES =

all: $(EPSFILES)

plot.case1.nc: mass2_v027.case1.cfg bcspecs.case1 stage.case1.dat grid1.dat \
	$(MODELFILES) $(MODEL)
	cp bcspecs.case1 bcspecs.dat
	cp mass2_v027.case1.cfg mass2.cfg
	$(MODEL)
	mv plot.nc $@

clean::
	rm -f plot.case1.nc mass2_v027.cfg

plot.case2.nc: mass2_v027.case2.cfg bcspecs.case2 stage.case2.dat grid2.dat \
	$(MODELFILES) $(MODEL)
	cp bcspecs.case2 bcspecs.dat
	cp mass2_v027.case2.cfg mass2.cfg
	$(MODEL)
	mv plot.nc $@

clean::
	rm -f plot.case2.nc mass2_v027.cfg

plot.case3.nc: mass2_v027.case3.cfg bcspecs.case1 stage.case1.dat grid2.dat \
	$(MODELFILES) $(MODEL)
	cp bcspecs.case1 bcspecs.dat
	cp mass2_v027.case3.cfg mass2.cfg
	$(MODEL)
	mv plot.nc $@

clean::
	rm -f plot.case3.nc mass2_v027.cfg

plot.case3blk.nc: mass2_v027.case3blk.cfg bcspecs.case3blk stage.case1.dat \
	grid1.dat grid3.dat $(MODELFILES) $(MODEL)
	cp bcspecs.case3blk bcspecs.dat
	cp mass2_v027.case3blk.cfg mass2_v027.cfg
	$(MODEL)
	mv plot.nc $@

clean::
	rm -f plot.case3blk.nc mass2_v027.cfg

plot.case1.eps: plot.case1.nc plot.gp
	sed -e 's/plot\.nc/plot.case1.nc/g' plot.gp | gnuplot > $@

plot.case2.eps: plot.case2.nc plot.gp
	sed -e 's/plot\.nc/plot.case2.nc/g' plot.gp | gnuplot > $@

plot.case3.eps: plot.case3.nc plot.gp
	sed -e 's/plot\.nc/plot.case3.nc/g' plot.gp | gnuplot > $@

plot.case3blk.eps: plot.case3blk.nc plot.gp
	sed -e 's/plot\.nc/plot.case3blk.nc/g' \
		-e 's/wsel 1 3/wsel 1 3 2 3/g' plot.gp | gnuplot > $@

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat bcspecs.dat
	rm -f plot.nc gage.nc
	rm -f $(EPSFILES)

