# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Mon May 19 12:18:42 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

SUBDIRS =										\
	1d-advection-1blk							\
	1d-analytic-1blk							\
	1d-analytic-2blk							\
	1d-analytic-3blk							\
	decay										\
	partition									\
	mixing										\
	mixing-transport-only						\
	mixing-2blk									\
	pointsource									\
	bedsource									\
	multi-bedsource								\
	bedpore

MODEL = ../../mass2
TECPLOT = tecplot

all: 
	set -e; \
	for d in $(SUBDIRS); do \
		$(MAKE) -C $$d MODEL=../$(MODEL) TECPLOT=$(TECPLOT) all; \
	done

clean: 
	set -e; \
	for d in $(SUBDIRS); do \
		$(MAKE) -C $$d MODEL=../$(MODEL) clean; \
	done

