# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 18, 2003 by William A. Perkins
# Last Change: Thu Apr 17 14:53:33 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

MODEL = ../../../mass2

TECPLOT=tecplot

EPSFILES = jda-cgns.eps jda-netcdf.eps

MODELFILES = \
	bcspecs.dat \
	jda-dense-pt.000.wide \
	mass2_v027.cfg \
	BCFiles/q100.dat \
	BCFiles/spillbybay.dat \
	BCFiles/stage158.4.dat 

all: $(EPSFILES)

plot.nc plot.cgns: $(MODELFILES) $(MODEL) 
	time $(MODEL)

jda-cgns.eps: jda-cgns.lay plot.cgns
	$(TECPLOT) -y $@ -b -p ../../../tecplot/macros/export_eps.mcr jda-cgns.lay

jda-netcdf.eps: jda-netcdf.lay plot.nc
	$(TECPLOT) -y $@ -b -p ../../../tecplot/macros/export_eps.mcr jda-netcdf.lay

clean:: 
	rm -f *~ batch.log tecplot.phy
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc grid.cgns plot*.cgns
	rm -f hotstart_*
	rm -f $(EPSFILES)
