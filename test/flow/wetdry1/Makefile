# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Wed Feb  6 06:54:09 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2_v027
TECPLOT = tecplot

EPSFILES = \
	plot_case1.eps \
	plot_case2.eps \
	plot_case3.eps

AVIFILES = \
	case1.avi \
	case2.avi \
	case3.avi

MODELFILES = \
	bcspecs.dat \
	flow.dat \
	wetdry-gridpt.001 \
	gage_control.dat \
	stage.dat \
	zero.dat

all: $(EPSFILES) $(AVIFILES)

plot_case1.eps: plot_case1.nc case1.lay
	$(TECPLOT) -b -y $@ case1.lay -p ../../../tecplot/macros/export_eps.mcr

case1.avi: plot_case1.nc case1.lay
	$(TECPLOT) -b -y $@ case1.lay -p avidump.mcr

plot_case2.eps: plot_case2.nc case2.lay
	$(TECPLOT) -b -y $@ case2.lay -p ../../../tecplot/macros/export_eps.mcr

case2.avi: plot_case2.nc case2.lay
	$(TECPLOT) -b -y $@ case2.lay -p avidump.mcr

plot_case3.eps: plot_case3.nc case3.lay
	$(TECPLOT) -b -y $@ case3.lay -p ../../../tecplot/macros/export_eps.mcr

case3.avi: plot_case3.nc case3.lay
	$(TECPLOT) -b -y $@ case3.lay -p avidump.mcr

hotstart_04-01-1996_070000.bin: $(MODELFILES) mass2_v027_warmup.cfg $(MODEL)
	cp mass2_v027_warmup.cfg mass2_v027.cfg
	$(MODEL)
	rm -f gage.nc plot.nc

plot_case1.nc gage_case1.nc hotstart_04-01-1996_080000.bin : $(MODELFILES) \
		mass2_v027_case1.cfg hotstart_04-01-1996_070000.bin $(MODEL)
	cp mass2_v027_case1.cfg mass2_v027.cfg
	cp hotstart_04-01-1996_070000.bin hotstart.bin
	$(MODEL)
	mv gage.nc gage_case1.nc
	mv plot.nc plot_case1.nc

plot_case2.nc gage_case2.nc: $(MODELFILES) mass2_v027_case2.cfg
	cp mass2_v027_case2.cfg mass2_v027.cfg $(MODEL)
	$(MODEL)
	mv gage.nc gage_case2.nc
	mv plot.nc plot_case2.nc

plot_case3.nc gage_case3.nc: $(MODELFILES) mass2_v027_case3.cfg \
		hotstart_04-01-1996_080000.bin $(MODEL)
	cp mass2_v027_case3.cfg mass2_v027.cfg
	cp hotstart_04-01-1996_080000.bin hotstart.bin
	$(MODEL)
	mv gage.nc gage_case3.nc
	mv plot.nc plot_case3.nc

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot_*.nc gage_*.nc
	rm -f batch.log
	rm -f hotstart*.bin
	rm -f mass2_v027.cfg
	rm -f $(EPSFILES)
