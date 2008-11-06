# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Wed Oct 29 11:46:53 2008 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

SUBDIRS =					\
	1d-advection-1blk			\
	1d-analytic-1blk			\
	1d-analytic-2blk			\
	1d-analytic-2blk-side			\
	1d-analytic-3blk			\
	1d-analytic-3blk-side			\
	decay					\
	partition				\
	mixing					\
	mixing-transport-only			\
	mixing-2blk				\
	skew45					\
	skew45-blk				\
	skew30					\
	skew30-blk				\
	pointsource				\
	bedsource				\
	multi-bedsource				\
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

