# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Tue Feb  5 09:22:56 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

SUBDIRS =										\
	normal-1blk									\
	normal-2blk									\
	slopebreak-1								\
	slopebreak-2								\
	normal-trap-1blk							\
	dead-1										\
	dead-1-blk									\
	dead-2										\
	dead-quickon								\
	wall-1										\
	wall-1-blk									\
	wall-2
# These could be added to the list, but take too long:
# jump
# wetdry1
# wetdry1-blk

MODEL = ../../mass2_v027
TECPLOT = tecplot

all: 
	set -e; \
	for d in $(SUBDIRS); do \
		$(MAKE) -C $$d MODEL=../$(MODEL) TECPLOT=$(TECPLOT) all; \
	done

clean: 
	set -e; \
	for d in $(SUBDIRS); do \
		$(MAKE) -C $$d MODEL=../$(MODEL) clean  ; \
	done

