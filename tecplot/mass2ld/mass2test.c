/* -------------------------------------------------------------
   file: mass2test.c
   ------------------------------------------------------------- */
/* -------------------------------------------------------------
   Battelle Memorial Institute
   Pacific Northwest Laboratory
   ------------------------------------------------------------- */
/* -------------------------------------------------------------
   Created August 18, 1999 by William A. Perkins
   Last Change: Tue Aug 24 12:01:34 1999 by William A. Perkins <perk@mack.pnl.gov>
   ------------------------------------------------------------- */


static const char* RCS_ID = "$Id$ Battelle PNL";

#include <stdio.h>
#include "mass2file.h"

const char *usage = "usage: %s file\n";

/* -------------------------------------------------------------
   Main Program
   ------------------------------------------------------------- */
int
main(int argc, char **argv)
{
  char name[1024];
  double values[309*59];
  float fvalues[308*59];

  if (argc <= 1) {
    fprintf(stderr, usage, argv[0]);
    exit(2);
  }
  if (mass2OpenFile(argv[1])) {
    fprintf(stderr, "error: %s: unable to open:\nerror: %s\n", argv[1], mass2_error);
    exit(1);
  } else {
    fprintf(stderr, "%s: OK\n", argv[1]);
  }

  mass2GetX(1, values);
  mass2GetY(1, values);
  mass2GetVar(0, 1, "zbot", fvalues);
  mass2GetVar(0, 1, "depth", fvalues);
  exit(0);
}
