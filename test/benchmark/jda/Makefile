# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 18, 2003 by William A. Perkins
# Last Change: Thu Nov  6 08:35:24 2008 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2

TECPLOT=tecplot

EPSFILES = jda-cgns.eps jda-netcdf.eps

MODELFILES = \
	bcspecs.dat \
	jda-dense-pt.000.wide \
	BCFiles/q100.dat \
	BCFiles/spillbybay.dat \
	BCFiles/stage158.4.dat 

all: $(EPSFILES)

hotstart.bin: $(MODELFILES) $(MODEL) mass2_v027.warmup
	cp mass2_v027.warmup mass2_v027.cfg
	$(MODEL)
	cp hotstart_04-03-2000_070000.bin hotstart.bin

plot.nc: $(MODELFILES) $(MODEL) mass2_v027.run hotstart.bin
	cp mass2_v027.run mass2_v027.cfg
	time $(MODEL)
	touch $@

plot000.cgns: plot.nc

jda-cgns.eps: jda-cgns.lay plot000.cgns
	$(TECPLOT) -y $@ -b -p ../../../tecplot/macros/export_eps.mcr jda-cgns.lay

jda-netcdf.eps: jda-netcdf.lay plot.nc
	$(TECPLOT) -y $@ -b -p ../../../tecplot/macros/export_eps.mcr jda-netcdf.lay

clean:: 
	rm -f *~ batch.log tecplot.phy
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc grid.cgns plot*.cgns
	rm -f hotstart_* hotstart.bin
	rm -f $(EPSFILES)
