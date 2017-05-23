# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: 2015-04-30 11:06:59 d3g096
# -------------------------------------------------------------
# $Id$

MPIRUN = mpirun
NPROC = 4
MODEL = ../../../mass2
PYTHON = python
GNUPLOT = gnuplot
EPSTOPDF = epstopdf

EPSFILES = \
	plot-depth.eps \
	plot-elev.eps

PDFFILES = $(EPSFILES:%.eps=%.pdf)

MODELFILES = \
	mass2.cfg \
	mass2.petscrc \
	bcspecs.dat \
	flow.dat \
	grid.dat \
	stage.dat \
	gage_control.dat

all: $(PDFFILES)

$(EPSFILES): plot000.cgns

plot000.cgns: $(MODELFILES) $(MODEL)
	$(MPIRUN) -np $(NPROC) $(MODEL)

%.eps: %.gp 
	PYTHON=$(PYTHON) $(GNUPLOT) $< > $@

%.pdf: %.eps
	$(EPSTOPDF) --filter < $< > $@

clean:: 
	rm -f *~
	rm -f error-warning*.out mass_source_monitor.out output*.out status*.out
	rm -f gridplot1.dat 
	rm -f plot000.cgns grid.cgns
	rm -f $(EPSFILES)

