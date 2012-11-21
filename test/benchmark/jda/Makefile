# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 18, 2003 by William A. Perkins
# Last Change: Mon Nov 19 08:54:46 2012 by William A. Perkins <d3g096@flophouse>
# -------------------------------------------------------------

MPIRUN = mpirun
NPROC = 8
MODEL = ../../../mass2

TECPLOT=tecplot

EPSFILES = jda-cgns.eps

MODELFILES = \
	bcspecs.dat \
	jda-dense-pt.000.wide \
	BCFiles/q100.dat \
	BCFiles/spillbybay.dat \
	BCFiles/stage158.4.dat 

all: $(EPSFILES)

hotstart.bin: $(MODELFILES) $(MODEL) mass2_v027.warmup
	cp mass2_v027.warmup mass2.cfg
	$(MPIRUN) -np $(NPROC) $(MODEL)
	cp hotstart_04-03-2000_070000.bin hotstart.bin

plot000.cgns: $(MODELFILES) $(MODEL) mass2_v027.run hotstart.bin
	cp mass2_v027.run mass2.cfg
	time $(MPIRUN) -np $(NPROC) $(MODEL)
	touch $@

jda-cgns.eps: jda-cgns.lay plot000.cgns
	$(TECPLOT) -y $@ -b -p ../../../tecplot/macros/export_eps.mcr jda-cgns.lay

clean:: 
	rm -f *~ batch.log tecplot.phy
	rm -f error-warning*.out mass_source_monitor.out output.out status*.out
	rm -f gage_block*.out
	rm -f mass2.cfg mass2_v027.cfg
	rm -f gridplot1.dat
	rm -f plot.nc grid.cgns plot*.cgns
	rm -f hotstart_* hotstart.bin
	rm -f $(EPSFILES)
