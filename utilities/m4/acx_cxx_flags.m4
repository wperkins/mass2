dnl -------------------------------------------------------------
dnl MY_CXX_FLAGS()

dnl This routine give the configure script several flags to control
dnl compilation: optimize, debug, inlining, and profiling.  Based on
dnl these flags, the appropriate C++ compiler flags are chosen for this
dnl project.

dnl -------------------------------------------------------------
AC_DEFUN([MY_CXX_FLAGS],
[

MY_CXXFLAGS=""

# Optimization options need to be compiler dependant (when we actually
# use different compilers).  For gcc, the flags "-O2 -funroll-loops
# -fstrict-aliasing -fno-gcse" come from the Blitz++ distribution.
# "-funroll-loops" appears cause problems with either the Boost
# libraries or the GNU stdc++ libraries (SEGV's in traversal of a list
# of shared pointers), so we take it out.

# GNU C++ flags always used: -ftemplate-depth-30 option is required by
# both blitz and MTL, -ftemplate-depth-50 is required by boost

my_cxx_required_flags="-ftemplate-depth-200 -Wall"

# GNU C++ flags used when functions are inlined. -DNDEBUG is for boost

my_cxx_inline_flags="-D_DO_INLINE_=inline -DNDEBUG"

# GNU C++ flags for optimized code. The flags "-O2 -funroll-loops
# -fstrict-aliasing -fno-gcse" come from the Blitz++ distribution.
# "-funroll-loops" appears cause problems with either the Boost
# libraries or the GNU stdc++ libraries (SEGV's in traversal of a list
# of shared pointers), so we take it out.

my_cxx_optimize_flags="-O2"

# GNU C++ flags for debugging.  These can be combined with my_cxx_optimize_flags. 

my_cxx_debug_flags="-g"

# GNU C++ flags for profiling.

my_cxx_profile_flags="-pg"

MY_CXX_DOOPTIMIZE=no
MY_CXX_DOINLINE=no
MY_CXX_DODEBUG=yes
MY_CXX_DOPROFILE=no

AC_ARG_ENABLE(optimize,
    AC_HELP_STRING([--enable-optimize], [turn on code optimization]),
    MY_CXX_DOOPTIMIZE=$enableval)
if test x"$MY_CXX_DOOPTIMIZE" = x"yes"; then
   MY_CXX_DODEBUG=no
   MY_CXX_DOINLINE=yes
fi

AC_ARG_ENABLE(profiling,
    AC_HELP_STRING([--enable-profiling], [turn on code profiling]),
    MY_CXX_DOPROFILE=$enableval)

AC_ARG_ENABLE(debug,
    AC_HELP_STRING([--enable-debug], [enable code debug (with --enable-optimize)]),
    MY_CXX_DODEBUG=$enableval)
if test x"$MY_CXX_DODEBUG" = x"yes"; then
   MY_CXX_DOINLINE=no
fi

AC_ARG_ENABLE(inline,
    AC_HELP_STRING([--(dis/en)able-inline], [turn (off/on) inlining (with --enable-(optimize/debug))]),
    MY_CXX_DOINLINE=$enableval)

my_cxx_extra_flags="$my_cxx_required_flags"

if test x"$MY_CXX_DODEBUG" = x"yes"; then
   my_cxx_extra_flags="$my_cxx_extra_flags $my_cxx_debug_flags"
fi
if test x"$MY_CXX_DOOPTIMIZE" = x"yes"; then
   my_cxx_extra_flags="$my_cxx_extra_flags $my_cxx_optimize_flags"
fi
if test x"$MY_CXX_DOINLINE" = x"yes"; then
   my_cxx_extra_flags="$my_cxx_extra_flags $my_cxx_inline_flags"
fi
if test x"$MY_CXX_DOPROFILE" = x"yes"; then
   my_cxx_extra_flags="$my_cxx_extra_flags $my_cxx_profile_flags"
fi

MY_CXXFLAGS="$MY_CXXFLAGS $my_cxx_extra_flags"
])