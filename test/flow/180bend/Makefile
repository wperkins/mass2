# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 26, 2004 by William A. Perkins
# Last Change: Fri Jul 17 12:48:44 2009 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

MODEL = ../../../mass2
MASS2SLICE = ../../../scripts/mass2slice.pl
TECPLOT = tecplot
PERL = perl

MESH = mesh.csv
MESH = coarse.csv 

    RBIDX = 1
    CLIDX = 11
    LBIDX = 22
ifeq ($(MESH), "coarse.csv" )
    RBIDX = 1
    CLIDX = 11
    LBIDX = 22
endif
ifeq ($(MESH), "mesh.csv")
    RBIDX = 1
    CLIDX = 20
    LBIDX = 39
endif

EPSFILES = \
	profile.eps

MODELFILES = \
	bcspecs.dat \
	gage_control.dat \
	$(MESH) \
	mass2_v027.cfg \
	stage.dat \
	flow.dat

all: $(EPSFILES)

$(EPSFILES): plot.nc

plot.nc: $(MODELFILES) $(MODEL)
	$(MODEL)

depth.cl.txt: plot.nc
	$(PERL) $(MASS2SLICE) -i -t 2 $< depth 1 $(CLIDX) > $@

depth.rb.txt: plot.nc
	$(PERL) $(MASS2SLICE) -i -t 2 $< depth 1 $(RBIDX) > $@

depth.lb.txt: plot.nc
	$(PERL) $(MASS2SLICE) -i -t 2 $< depth 1 $(LBIDX) > $@

depth.txt: depth.cl.txt depth.rb.txt depth.lb.txt
	paste depth.cl.txt depth.rb.txt depth.lb.txt > $@

profile.eps: plot.gp depth.txt
	gnuplot plot.gp > $@
clean::
	rm -f *~
	rm -f hotstart*.bin
	rm -f error-warning.out mass_source_monitor.out output.out status.out
	rm -f gridplot1.dat
	rm -f plot.nc grid.cgns plot000.cgns
	rm -f depth.txt depth.cl.txt depth.rb.txt depth.lb.txt
	rm -f batch.log
	rm -f $(EPSFILES)
