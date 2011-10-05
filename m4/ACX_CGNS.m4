# -*- Autoconf -*-

dnl ----------------------------------------------------------------------------
dnl ACX_CGNS77: check for the required CGNS Fortran library
dnl ----------------------------------------------------------------------------
AC_DEFUN([ACX_CGNS77], [
AC_REQUIRE([AC_CANONICAL_HOST]) 
AC_REQUIRE([AC_PROG_CPP])
AC_REQUIRE([AC_PROG_FC])

                                # CGNS location: set CGNSDIR or --with-cgns=

acx_cgns_dir=
AC_ARG_WITH(cgns,
    [  --with-cgns=dir              specify the location of CGNS libraries],
    acx_cgns_dir="$withval")

acx_cgns_cppflags=
if test -n "$acx_cgns_dir"; then
    if test -d "$acx_cgns_dir/include"; then
        acx_cgns_cppflags="-I$acx_cgns_dir/include"
    else
        acx_cgns_cppflags="-I$acx_cgns_dir"
    fi
fi

acx_cgns_target=unknown
case $host in
    x86_64*linux*)
        acx_cgns_target=LINUX64
        ;;
    i?86*linux*)
        acx_cgns_target=LINUX
        ;;
    *apple-darwin*)
        acx_cgns_target=DARWIN
        ;;
    *)
        ;;
esac

acx_cgns_ldflags=
if test -n "$acx_cgns_dir"; then
    if test -d "$acx_cgns_dir/$acx_cgns_target"; then
        acx_cgns_ldflags="-L$acx_cgns_dir/$acx_cgns_target"
    else
        acx_cgns_ldflags="-L$acx_cgns_dir/lib"
    fi
fi
acx_cgns_ldflags="$acx_cgns_ldflags -lcgns"

acx_cgns_finc="cgnslib_f.h"
acx_cgns_ok=yes
tmp_FCFLAGS="$FCFLAGS"
tmp_LDFLAGS="$LDFLAGS"
AC_LANG_PUSH([Fortran 77])
FCFLAGS="$FCFLAGS $acx_cgns_cppflags"
LDFLAGS="$LDFLAGS $acx_cgns_ldflags"
AC_MSG_CHECKING([checking CGNS Fortran include file $acx_cgns_finc])
AC_COMPILE_IFELSE(
    [AC_LANG_PROGRAM([], [#include "$acx_cgns_finc"])],
    [AC_MSG_RESULT([yes])],
    [acx_cgns_ok=no; AC_MSG_RESULT([no])])
                
AC_CHECK_LIB(cgns, cg_open_f, ,
             [acx_cgns_ok=no])
AC_LANG_POP()
FCFLAGS=$tmp_FCFLAGS
LDFLAGS=$tmp_LDFLAGS

if test x$acx_cgns_ok != xyes; then
    AC_MSG_ERROR([Problem with CGNS])   
fi

CGNS_FCFLAGS="$acx_cgns_cppflags"
CGNS_LIBS="$acx_cgns_ldflags"

AC_SUBST(CGNS_FCFLAGS)
AC_SUBST(CGNS_LIBS)

])
