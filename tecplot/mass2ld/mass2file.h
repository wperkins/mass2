/* -------------------------------------------------------------
   file: mass2file.h
   ------------------------------------------------------------- */
/* -------------------------------------------------------------
   Battelle Memorial Institute
   Pacific Northwest Laboratory
   ------------------------------------------------------------- */
/* -------------------------------------------------------------
   Created August 18, 1999 by William A. Perkins
   Last Change: Tue Aug 24 10:11:43 1999 by William A. Perkins <perk@mack.pnl.gov>
   ------------------------------------------------------------- */

/* $Id$ */

#ifndef _mass2file_h_
#define _mass2file_h_


extern char mass2_error[];
extern char mass2_file[];
extern int mass2_ncid;

                         /* non-time dependant variables (optional) */

extern int mass2_static_vars;
extern int mass2_static_varid[];

                         /* time dependant variables */

extern int mass2_timedep_vars;
extern int mass2_timedep_varid[];


                                /* function protytypes */

int mass2OpenFile(const char *name);
int mass2CloseFile(void);

int mass2CheckVar(int ncid, const char *varname);
int mass2CheckDim(int ncid, const char *dimname);
const char *mass2VarName(const int id);
const char *mass2VarDesc(const int id);

int mass2Times(void);
int mass2Blocks(void);
int mass2MaxEta(void);
int mass2MaxXi(void);
int mass2BlkEta(const int block);
int mass2BlkXi(const int block);
int mass2GetX(const int block, double *values);
int mass2GetY(const int block, double *values);
int mass2GetVar(const int timeidx, const int block, 
                const char *varname, float *values);

const char *mass2TimeStamp(const int i);


#endif
