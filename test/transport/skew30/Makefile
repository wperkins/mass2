# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 14, 2002 by William A. Perkins
# Last Change: Mon Oct 22 09:59:17 2012 by William A. Perkins <d3g096@flophouse>
# -------------------------------------------------------------

MODEL = ../../../mass2
TECPLOT = tecplot360

CASES = UPWIND CDS SOU MSOU MUSCL

EPSFILES = $(CASES:%=plot-%.eps)
PLOTFILES = $(CASES:%=plot-%.cgns)
PROFFILES = $(CASES:%=plot-%.dat)

MODELFILES = \
	bcspecs.dat \
	grid1.dat \
	scalar_bcspecs.dat \
	scalar_source.base \
	conc.dat \
	zero.dat

all: $(EPSFILES)

$(PLOTFILES): plot-%.cgns: $(MODELFILES) $(MODEL) mass2_v027.base hotstart.bin
	cp mass2_v027.base mass2_v027.cfg
	sed -e "s/@S@/$*/g" scalar_source.base > scalar_source.dat
	$(MODEL)
	mv plot000.cgns $@

$(EPSFILES): plot-%.eps: plot-%.cgns plot.lay
	sed -e "s/plot000.cgns/plot-$*.cgns/g" plot.lay > junk.lay
	$(TECPLOT) -y $@ -b -p ../../../tecplot/macros/export_eps.mcr junk.lay

hotstart.bin: $(MODELFILES) $(MODEL) mass2_v027.hotstart
	cp mass2_v027.hotstart mass2_v027.cfg
	$(MODEL)
	mv hotstart_01-01-1997_050001.bin hotstart.bin

clean:: 
	rm -f *~
	rm -f error-warning.out mass_source_monitor.out output.out status.out stuff_balance.out
	rm -f gridplot1.dat 
	rm -f plot.nc gage.nc scalar_source.dat mass2_v027.cfg
	rm -f grid.cgns plot000.cgns hotstart.bin
	rm -f $(PLOTFILES) junk.lay
	rm -f $(EPSFILES)
