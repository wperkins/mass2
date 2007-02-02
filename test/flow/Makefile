# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Mon May  3 07:28:24 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

SUBDIRS =										\
	normal-1blk									\
	normal-1blk-side							\
	normal-2blk									\
	normal-2blk-side							\
	normal-3blk									\
	normal-3blk-side							\
	slopebreak-1								\
	slopebreak-2								\
	normal-trap-1blk							\
	normal-super								\
	dead-1										\
	dead-1-blk									\
    dead-1-blk-nest								\
    dead-1-blk-nest-side						\
	dead-2										\
	dead-2-blk									\
	dead-2-blk-nest								\
	dead-quickon								\
	wall-1										\
	wall-1-blk									\
	wall-2										\
	MacDonald-3									\
	MacDonald-4									\
	Rodi										\
	Soulis										\
	spur-dike-1									\
	meander										\
	jump										\
	another-jump

# These could be added to the list, but take too long:
# wetdry1
# wetdry1-blk

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
		$(MAKE) -C $$d MODEL=../$(MODEL) clean  ; \
	done

