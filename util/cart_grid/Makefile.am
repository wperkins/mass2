# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created April  1, 2003 by William A. Perkins
# Last Change: Tue Apr  1 15:33:17 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

SUBDIRS = 
bin_PROGRAMS = cartgrid

cartgrid_SOURCES = \
	 cartgrid.f90								\
	 elevtbl.f90 

# dependancies for individual object files

cartgrid.o: cartgrid.f90 elevtbl.o
elevtbl.o: elevtbl.f90


EXTRA_DIST = \
	cart_grid.mak \
	cart_grid.dsp 