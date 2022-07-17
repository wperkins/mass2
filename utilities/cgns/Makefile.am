# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 22, 2003 by William A. Perkins
# Last Change: 2018-10-05 06:43:14 d3g096
# -------------------------------------------------------------
# $Id$

SUBDIRS = .

ACLOCAL_AMFLAGS = -I ../m4 

AM_YFLAGS = -d

AM_CPPFLAGS = \
    -DBOOST_FILESYSTEM_VERSION=3

LIBRARY = libcgnsfile.a
lib_LIBRARIES = $(LIBRARY)

bin_PROGRAMS  = \
	cgnsvtk \
	transient \
	cgnsscale \
	starbnd \
	starvrt \
	cgnszconv \
	cgnsextract \
	cgnssplit \
	cgnssplitindex \
	copydiscrete \
	cgnsaverage \
	cgnscopyfield \
	cgnszonegraph \
	cgnsopen \
	cgnsbc

check_PROGRAMS = \
	cgnstest \
	cylinder \
	multimixed

CGNSFILESRC = \
	cgnsfile.h cgnsfile.cpp

libcgnsfile_a_SOURCES = $(CGNSFILESRC)

cgnstest_SOURCES = cgnstest.cpp
cgnstest_LDADD = $(LIBRARY) $(CGNSLIB) 

cylinder_SOURCES = cylinder.cpp
cylinder_LDADD = $(LIBRARY) $(CGNSLIB) 

cgnsvtk_SOURCES = cgnsvtk.cpp
cgnsvtk_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB)

multimixed_SOURCES = multimixed.cpp
multimixed_LDADD = $(LIBRARY) $(CGNSLIB) 

transient_SOURCES = transient.cpp
transient_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB) 

cgnsscale_SOURCES = cgnsscale.cpp
cgnsscale_LDADD = $(LIBRARY) $(CGNSLIB) 

starbnd_SOURCES = starbnd.cpp
starbnd_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB) 

starvrt_SOURCES = starvrt.cpp
starvrt_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB) 

cgnszconv_SOURCES = cgnszconv.cpp
cgnszconv_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB) 

cgnsextract_SOURCES = cgnsextract.cpp
cgnsextract_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB) 

cgnssplit_SOURCES = cgnssplit.cpp
cgnssplit_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB) 

cgnssplitindex_SOURCES = cgnssplitindex.cpp
cgnssplitindex_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB) 

copydiscrete_SOURCES = copydiscrete.cpp
copydiscrete_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB) 

cgnsaverage_SOURCES = cgnsaverage.cpp
cgnsaverage_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB) 

cgnscopyfield_SOURCES = cgnscopyfield.cpp
cgnscopyfield_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB)

cgnszonegraph_SOURCES = cgnszonegraph.cpp
cgnszonegraph_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB)

cgnsopen_SOURCES = cgnsopen.c
cgnsopen_LDADD = $(CGNSLIB)

cgnsbc_SOURCES = cgnsbc.cpp
cgnsbc_LDADD = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_SYSTEM_LIB) $(LIBRARY) $(CGNSLIB)

EXTRA_DIST =  \
	saved_svn_version.txt \
	saved_svn_version_date.txt

doxygen:
	$(MAKE) -C doc doxygen

clean-local:
	rm -f *~
