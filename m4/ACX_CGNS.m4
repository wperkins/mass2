# -*- Autoconf -*-

dnl ----------------------------------------------------------------------------
dnl ACX_CGNS77: check for the required CGNS Fortran library
dnl ----------------------------------------------------------------------------
AC_DEFUN([ACX_CGNS77], [
AC_REQUIRE([AC_CANONICAL_HOST]) 

                                # CGNS location: set CGNSDIR or --with-cgns=

if test -z "$CGNSDIR"; then
    CGNSDIR=/usr/unsupported/CGNSLib
fi
AC_ARG_WITH(cgns,
    [  --with-cgns=dir              specify the location of CGNS libraries],
    CGNSDIR="$withval")

AC_ARG_WITH(cgns-lib,
    [  --with-cgns-lib=name         name of CGNS library: libcgns.a = cgns],
    CGNSLIBNAME="$withval")

CGNS_LIBS_PATH="$CGNSDIR/lib"
CGNS_INCLUDE_PATH="$CGNSDIR"

                                # some of these names for the CGNS
                                # library are site specific
if test -z "$CGNSLIBNAME"; then
    case $host in
        i?86*linux*)
            CGNSLIBNAME=cgns.LINUX
            ;;
        alpha*linux*)
            CGNSLIBNAME=cgns.alinux
            ;;
        alphaev*-dec-osf*)
            CGNSLIBNAME=cgns.ALPHA
            ;;
        mips-*-irix*)
            CGNSLIBNAME=cgns.mips4_n32
            ;;
        *apple-darwin*)
            CGNSLIBNAME="cgns.DARWIN"
            ;;
        *)
            CGNSLIBNAME=cgns
    esac
fi
CGNS_LIBS="-l$CGNSLIBNAME"

AC_LANG_PUSH(Fortran)
acx_cgns77_FCFLAGS="$FCFLAGS"
acx_cgns77_LDFLAGS="$LDFLAGS"
acx_cgns77_LIBS="$LIBS"

FCFLAGS="$FCFLAGS -I$CGNS_INCLUDE_PATH"
LDFLAGS="$LDFLAGS -L$CGNS_LIBS_PATH"

AC_CHECK_FILE($CGNSDIR/cgnslib_f.h, , 
              AC_MSG_ERROR(Cannot find required header cgnslib_f.h))
AC_CHECK_LIB($CGNSLIBNAME, cg_open_f, ,
             AC_MSG_ERROR(Cannot link to CGNS library))

FCFLAGS="$acx_cgns77_FCFLAGS"
LDFLAGS="$acx_cgns77_LDFLAGS"
LIBS="$acx_cgns77_LIBS"

AC_LANG_POP(Fortran)

AC_SUBST(CGNS_INCLUDE_PATH)
AC_SUBST(CGNS_LIBS_PATH)
AC_SUBST(CGNS_LIBS)

])
