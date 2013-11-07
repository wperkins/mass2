# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 10, 2010 by William A. Perkins
# Last Change: 2013-11-07 06:18:23 d3g096
# -------------------------------------------------------------

SUBDIRS =

AM_FCFLAGS = 

lib_LIBRARIES = libgastemp.a

libgastemp_a_SOURCES = \
	gas_coeffs_module.f90 \
	gas_functions_module.f90 \
	energy_flux_module.f90

gas_functions_module.$(OBJEXT): gas_coeffs_module.$(OBJEXT) 

clean-local:
	rm -f *.$(F90MODULEEXT)
	rm -f *.il work.*
	rm -f *~
