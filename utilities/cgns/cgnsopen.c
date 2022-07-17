/* -------------------------------------------------------------
   file: cgnsopen.c
   ------------------------------------------------------------- */
/* -------------------------------------------------------------
   Battelle Memorial Institute
   Pacific Northwest Laboratory
   ------------------------------------------------------------- */
/* -------------------------------------------------------------
   Created August  9, 2010 by William A. Perkins
   Last Change: Fri Dec  2 14:31:04 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
   ------------------------------------------------------------- */


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <stdio.h>
#include <stdlib.h>
#include <cgnslib.h>

/* -------------------------------------------------------------
   do_or_die
   ------------------------------------------------------------- */
void
do_or_die(int ierr, const char* msg)
{
  if (ierr != CG_OK) {
    fprintf(stderr, "error: %s: %s\n", msg, cg_get_error());
    exit(1);
  }
}

/* -------------------------------------------------------------
   Main Program
   ------------------------------------------------------------- */
int
main(int argc, char **argv)
{
  int fileidx;
  int bases;
  int ierr;

  if (argc < 2) {
    fprintf(stderr, "usage: %s file\n", argv[0]);
    exit(3);
  }

  ierr = cg_open(argv[1], CG_MODE_READ, &fileidx);
  do_or_die(ierr, "open file");

  ierr = cg_nbases(fileidx, &bases); 
  do_or_die(ierr, "count bases");

  if (bases > 0) {
    int dimp, dimc;
    char buf[128];
    ierr = cg_base_read(fileidx, 1, &buf[0], &dimc, &dimp);
    do_or_die(ierr, "read base");
  }
  ierr = cg_close(fileidx);
  do_or_die(ierr, "close file");

  fprintf(stderr, "done\n");

    return 0;
}
