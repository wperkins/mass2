# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Wed Feb  6 07:11:37 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

SUBDIRS =										\
	seepdry										\
	normal

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
		$(MAKE) -C $$d MODEL=../$(MODEL) clean; \
	done

