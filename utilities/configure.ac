dnl Process this file with autoconf to produce a configure script.
AC_INIT()
AM_INIT_AUTOMAKE(mass2utility,0.0)
AM_MAINTAINER_MODE()

dnl Checks for programs.
AC_CANONICAL_HOST

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T
AC_STRUCT_TM

AC_LANG(C++)

dnl variables to pass on to Makefile.in and config.h.in

AC_SUBST(CGNSINC)
AC_SUBST(CGNSLIB)

dnl Process command-line arguments for configure

AC_ARG_WITH(cgns,
  [  --with-cgns=DIR              set the path for the CGNS library],
  CGNSINC="-I$withval -I$withval/include"
  CGNSLIB="-L$withval/lib",
  CGNSINC=""
  CGNSLIB="")
CPPFLAGS="$CGNSINC $CPPFLAGS"

cgns_lib_name=""
AC_ARG_WITH(cgns-lib,
    AC_HELP_STRING([--with-cgns-lib=name], [Specify the name of the cgns library (default="cgns")]),
    cgns_lib_name=$withval,
    cgns_lib_name="")
if test -z "$cgns_lib_name"; then    
   case $host in                
        *linux*)
            cgns_lib_name="cgns.LINUX"
            ;;
        *apple-darwin*)
            cgns_lib_name="cgns.DARWIN"
            ;;
        *)
            cgns_lib_name="cgns"
            ;;
    esac
fi
CGNSLIB="$CGNSLIB  -l$cgns_lib_name"

CXXFLAGS="$CXXFLAGS $MY_CXXFLAGS"

# Check the Boost Library
AX_BOOST_BASE([1.47],
              [],
              [AC_MSG_ERROR(Recent Boost libraries are required)])
AX_BOOST_PROGRAM_OPTIONS()
AX_BOOST_FILESYSTEM()
AX_BOOST_SYSTEM()
AX_BOOST_DATE_TIME()

CPPFLAGS="$BOOST_CPPFLAGS $CPPFLAGS"
LDFLAGS="$LDFLAGS $BOOST_LDFLAGS"

dnl Checks for header files.

AC_CHECK_HEADER(cgnslib.h, AC_DEFINE(HAVE_CGNS),
    AC_MSG_ERROR(TE2THYS requires the CGNS Library))

# Check for the Blitz++ library, version 0.10. Only the tiny vector
# and matrix libraries are used, so check to make sure it's the right
# one

AC_CHECK_BLITZ()
CPPFLAGS="$BLITZ_CPPFLAGS $CPPFLAGS"
LDFLAGS="$BLITZ_LDFLAGS $LDFLAGS"

dnl Checks for library functions.

AC_CONFIG_SUBDIRS([cgns])
AC_CONFIG_FILES([ Makefile ])
AC_OUTPUT()
