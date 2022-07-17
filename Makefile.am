# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 22, 2003 by William A. Perkins
# -------------------------------------------------------------
# $Id: Makefile.am 2660 2015-04-10 22:51:46Z d3g096@PNL.GOV $

AM_YFLAGS = -d

ACLOCAL_AMFLAGS = -I m4

SUBDIRS = cgns 

AM_CPPFLAGS = \
    -DBOOST_FILESYSTEM_VERSION=3 \
    -I$(srcdir)/cgns

CGNSFILELIB = ./cgns/libcgnsfile.a

bin_PROGRAMS = \
	mass2flux \
	mass2stats \
	mass2grass \
	mass2insert \
	mass2scan \
	mass2extract \
	mass2wet \
	mass2dewater

mass2flux_SOURCES = mass2flux.cpp
mass2flux_LDADD = \
	$(CGNSFILELIB) $(CGNSLIB) \
	$(BOOST_PROGRAM_OPTIONS_LIB) \
	$(BOOST_FILESYSTEM_LIB) \
	$(BOOST_SYSTEM_LIB) \
	$(BLITZ_LIBS)

mass2stats_SOURCES = mass2stats.cpp \
	mass2series.h mass2series.cpp \
	mass2solution.h mass2solution.cpp
mass2stats_LDADD = \
	$(CGNSFILELIB) $(CGNSLIB) \
	$(BOOST_PROGRAM_OPTIONS_LIB) \
	$(BOOST_FILESYSTEM_LIB) \
	$(BOOST_SYSTEM_LIB) \
	$(BLITZ_LIBS)

mass2grass_SOURCES = mass2grass.cpp \
	mass2series.h mass2series.cpp \
	mass2solution.h mass2solution.cpp
mass2grass_LDADD = \
	$(CGNSFILELIB) $(CGNSLIB) \
	$(BOOST_PROGRAM_OPTIONS_LIB) \
	$(BOOST_FILESYSTEM_LIB) \
	$(BOOST_SYSTEM_LIB) \
	$(BLITZ_LIBS)

mass2insert_SOURCES = mass2insert.cpp \
	mass2solution.h mass2solution.cpp
mass2insert_LDADD = \
	$(CGNSFILELIB) $(CGNSLIB) \
	$(BOOST_PROGRAM_OPTIONS_LIB) \
	$(BOOST_FILESYSTEM_LIB) \
	$(BOOST_SYSTEM_LIB) \
	$(BLITZ_LIBS)

mass2scan_SOURCES = mass2scan.cpp mass2solution.cpp mass2series.cpp DateTime.cpp
mass2scan_LDADD = \
	$(CGNSFILELIB) $(CGNSLIB) \
	$(BOOST_PROGRAM_OPTIONS_LIB) \
	$(BOOST_FILESYSTEM_LIB) \
	$(BOOST_SYSTEM_LIB) \
	$(BLITZ_LIBS)

mass2extract_SOURCES = mass2extract.cpp  \
	mass2series.h mass2series.cpp \
	mass2solution.h mass2solution.cpp
mass2extract_LDADD = \
	$(CGNSFILELIB) $(CGNSLIB) \
	$(BOOST_PROGRAM_OPTIONS_LIB) \
	$(BOOST_FILESYSTEM_LIB) \
	$(BOOST_SYSTEM_LIB)

mass2wet_SOURCES = mass2wet.cpp DateTime.cpp \
	mass2series.cpp mass2solution.cpp
mass2wet_LDADD = \
	$(CGNSFILELIB) $(CGNSLIB) \
	$(BOOST_PROGRAM_OPTIONS_LIB) \
	$(BOOST_FILESYSTEM_LIB) \
	$(BOOST_SYSTEM_LIB)

mass2dewater_SOURCES = mass2dewater.cpp DateTime.cpp \
	mass2series.cpp mass2solution.cpp
mass2dewater_LDADD = \
	$(CGNSFILELIB) $(CGNSLIB) \
	$(BOOST_PROGRAM_OPTIONS_LIB) \
	$(BOOST_FILESYSTEM_LIB) \
	$(BOOST_SYSTEM_LIB)


clean-local:
	rm -f *~
