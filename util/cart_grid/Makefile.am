# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created April  1, 2003 by William A. Perkins
# Last Change: Wed Apr  2 07:16:28 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

AM_FFLAGS = $(FCFLAGS)

SUBDIRS = 
bin_PROGRAMS = cartgrid

cartgrid_SOURCES = \
	 cartgrid.f90								\
	 elevtbl.f90 

# dependancies for individual object files

cartgrid.o: cartgrid.f90 elevtbl.o
elevtbl.o: elevtbl.f90

clean-local:
	rm -f *.$(F90MODULEEXT)
	rm -f *~

EXTRA_DIST = \
	makefile.linux \
	makefile.sgi \
	makefile.dec \
	cart_grid.mak \
	cart_grid.dsp 

