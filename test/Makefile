# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: Wed Feb  6 07:10:35 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$


SUBDIRS = \
	flow \
	transport \
    src-sink \
 	sediment 

MODEL = ../mass2_v027
TECPLOT = tecplot

all: 
	set -e; \
	for d in $(SUBDIRS); do \
		$(MAKE) -C $$d MODEL=../$(MODEL) TECPLOT=$(TECPLOT) all; \
	done

clean: 
	for d in $(SUBDIRS); do \
		$(MAKE) -C $$d MODEL=../$(MODEL) clean ; \
	done
