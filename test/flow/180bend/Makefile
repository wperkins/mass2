# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 26, 2004 by William A. Perkins
# Last Change: Mon Nov 19 13:34:17 2012 by William A. Perkins <d3g096@flophouse>
# -------------------------------------------------------------
# $Id$

MPIRUN = mpirun
NPROC = 4
MODEL = ../../../mass2

MASS2SLICE = ../../../scripts/mass2slice.pl
TECPLOT = tecplot
PYTHON = python

MESH = mesh.csv
# MESH = coarse.csv 

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
	plot.eps \
	profile.eps

MODELFILES = \
	bcspecs.dat \
	gage_control.dat \
	$(MESH) \
	mass2_v027.cfg \
	stage.dat \
	flow.dat

all: $(EPSFILES)

$(EPSFILES): plot000.cgns

plot000.cgns: $(MODELFILES) $(MODEL)
	cp mass2_v027.cfg mass2.cfg
	$(MPIRUN) -np $(NPROC) $(MODEL)

depth.txt: plot000.cgns profile.py
	$(PYTHON) profile.py --output=$@ plot000.cgns

profile.eps: plot.gp depth.txt
	gnuplot plot.gp > $@

plot.eps: plot000.cgns plot.lay
	$(TECPLOT) -b -y $@ plot.lay -p ../../../tecplot/macros/export_eps.mcr

clean::
	rm -f *~
	rm -f hotstart*.bin
	rm -f error-warning*.out mass_source_monitor.out output.out status*.out
	rm -f gridplot1.dat
	rm -f plot.nc grid.cgns plot000.cgns
	rm -f depth.txt depth.cl.txt depth.rb.txt depth.lb.txt
	rm -f batch.log mass2.cfg
	rm -f $(EPSFILES)
