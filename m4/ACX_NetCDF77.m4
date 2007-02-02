dnl ----------------------------------------------------------------------------
dnl ACX_NetCDF77: check for the required NetCDF Fortran library
dnl ----------------------------------------------------------------------------
AC_DEFUN([ACX_NetCDF77], [

dnl NetCDF location: set NETCDFDIR or --with-netcdf=

AC_ARG_WITH(netcdf,
    [  --with-netcdf=dir            specify the location of NetCDF libraries],
    NETCDFDIR="$withval")
if test -z "$NETCDFDIR"; then
    NETCDFDIR=/usr
fi

NETCDF_LIBS="-lnetcdf"
NETCDF_LIBS_PATH="$NETCDFDIR/lib"
NETCDF_INCLUDES_PATH="$NETCDFDIR/include"

acx_netcdf77_FCFLAGS="$FCFLAGS"
acx_netcdf77_LIBS="$LIBS"
acx_netcdf77_LDFLAGS="$LDFLAGS"

AC_LANG_PUSH(Fortran)

FCFLAGS="$FCFLAGS -I$NETCDF_INCLUDES_PATH"
LIBS="$LIBS $NETCDF_LIBS"
LDFLAGS="$LDFLAGS -L$NETCDF_LIBS_PATH"

dnl look for the fortran include file, and try to link to library
AC_CHECK_FILE($NETCDFDIR/include/netcdf.inc, , AC_MSG_ERROR(Cannot find required header netcdf.inc))
AC_CHECK_LIB(netcdf, nf_create, , AC_MSG_ERROR(Cannot link to NetCDF library))

FCFLAGS="$acx_netcdf77_FCFLAGS"
LIBS="$acx_netcdf77_LIBS"
LDFLAGS="$acx_netcdf77_LDFLAGS"

AC_LANG_POP(Fortran)

AC_SUBST(NETCDF_LIBS)
AC_SUBST(NETCDF_LIBS_PATH)
AC_SUBST(NETCDF_INCLUDES_PATH)

]) dnl ACX_NetCDF
