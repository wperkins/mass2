/* -------------------------------------------------------------
   file: mass2file.c
   ------------------------------------------------------------- */
/* -------------------------------------------------------------
   Battelle Memorial Institute
   Pacific Northwest Laboratory
   ------------------------------------------------------------- */
/* -------------------------------------------------------------
   Created August 17, 1999 by William A. Perkins
   Last Change: Tue Aug 24 12:01:23 1999 by William A. Perkins <perk@mack.pnl.gov>
   ------------------------------------------------------------- */


static const char* RCS_ID = "$Id$ Battelle PNL";

#include <stdio.h>
#include <string.h>
#include <netcdf.h>
#include "mass2file.h"

char mass2_error[1024];         /* exported */

char mass2_file[1024];
int mass2_ncid;

                                /* Dimension id's (must exist) */

int mass2_blk_dimid;
int mass2_eta_dimid;
int mass2_xi_dimid;
int mass2_ts_dimid;
int mass2_time_dimid;

                                /* known variable id's (must exist) */

int mass2_etamax_varid;
int mass2_ximax_varid;
int mass2_time_varid;
int mass2_x_varid;
int mass2_y_varid;
int mass2_ts_varid;

                                /* non-time dependant variables (optional) */

int mass2_static_vars = 0;
int mass2_static_varid[NC_MAX_VARS];

                                /* time dependant variables */

int mass2_timedep_vars = 0;
int mass2_timedep_varid[NC_MAX_VARS];

static char buffer[10240];

/* -------------------------------------------------------------
   mass2VarName
   ------------------------------------------------------------- */
const char * 
mass2VarName(const int id)
{
  int ncstat = nc_inq_varname(mass2_ncid, id, buffer);
  return buffer;
}

/* -------------------------------------------------------------
   mass2VarDesc
   ------------------------------------------------------------- */
const char * 
mass2VarDesc(const int id)
{
  nc_type ntype;
  int len;
  int ncstat = 0;

  strcpy(buffer, "");
  ncstat = nc_get_att_text(mass2_ncid, id, "Description", buffer);
  return buffer;
}

/* -------------------------------------------------------------
   mass2CheckVar
   Checks to see if the named variable exists in the netcdf file. 
   If so, its id is returned, otherwise a message box is displayed and 
   -1 is returned
   ------------------------------------------------------------- */
int
mass2CheckVar(int ncid, const char *varname)
{
  int varid;
  int ncstat = nc_inq_varid(ncid, varname, &varid);
  int err = 0;

  if (ncstat != NC_NOERR) {
    sprintf(mass2_file, "Not a MASS2 file?: cannot find variable \"%s\": \n %s",
            varname, nc_strerror(ncstat));
    err++;
  }
  return (err ? -1 : varid);
}

/* -------------------------------------------------------------
   mass2CheckDim
   Checks to see if the named dimension exists in the netcdf file. 
   If so, its id is returned, otherwise a message box is displayed and 
   -1 is returned
   ------------------------------------------------------------- */
int
mass2CheckDim(int ncid, const char *dimname)
{
  int dimid;
  int ncstat = nc_inq_dimid(ncid, dimname, &dimid);
  int err = 0;

  if (ncstat != NC_NOERR) {
    sprintf(mass2_error, "Not a MASS2 file?: cannot find dimension \"%s\": \n %s",
            dimname, nc_strerror(ncstat));
    err++;
  }
  return (err ? -1 : dimid);
}
/* -------------------------------------------------------------
   mass2ldGetVars
   This routine goes through the vars in the NetCDF file and 
   determines if they are spatial and/or time-dependant
   ------------------------------------------------------------- */
static void
mass2ldGetVars(int ncid)
{
  int nvars, ncstat = 0;
  int id;

  mass2_timedep_vars = 0;
  mass2_static_vars = 0;

  ncstat = nc_inq_nvars(ncid, &nvars);

  for (id = 0; id < nvars; id++) {
    char vname[256];
    nc_type vtype;
    int ndim, dimids[NC_MAX_VAR_DIMS], natts;
    int d;
    int tstspc = 0;
    int tsttim = 0;
    
    ncstat = nc_inq_var(ncid, id, vname, &vtype, &ndim, dimids, &natts);
    
    if (id == mass2_etamax_varid || 
        id == mass2_ximax_varid ||
        id == mass2_time_varid ||
        id == mass2_x_varid ||
        id == mass2_y_varid ||
        id == mass2_ts_varid) continue;
        
    if (ndim < 3) continue;


                                /* it's a spatial variable if it has
                                   three dimensions: block, eta, and xi */

    for (d = 0; d < ndim; d++) {
      if (d == mass2_blk_dimid || d == mass2_eta_dimid || d == mass2_xi_dimid)
        tstspc += 1;
      if (d == mass2_time_dimid) tsttim += 1;
    }
    if (tstspc == 3 && tsttim) {
      mass2_timedep_varid[mass2_timedep_vars] = id;
      mass2_timedep_vars++;
    } else if (tstspc == 3) {
      mass2_static_varid[mass2_static_vars] = id;
      mass2_static_vars++;
    }
  }
}


/* -------------------------------------------------------------
   mass2OpenFile
   This routine opens and checks the validity of a MASS2 plot output file.
   ------------------------------------------------------------- */
int 
mass2OpenFile(const char *name)
{
  int ncstat, ncid;
  int err = 0;

  ncstat = nc_open(name, (NC_NOWRITE | NC_SHARE), &ncid);
  if (ncstat != NC_NOERR) {
    sprintf(mass2_error, "Unable to open file \"%s\": \n%s", name, 
            nc_strerror(ncstat));
    err++;
  }

  if (! err) {
    if ((mass2_blk_dimid = mass2CheckDim(ncid, "block")) < 0) err++;
    if ((mass2_eta_dimid = mass2CheckDim(ncid, "eta")) < 0) err++;
    if ((mass2_xi_dimid = mass2CheckDim(ncid, "xi")) < 0) err++;
    if ((mass2_ts_dimid = mass2CheckDim(ncid, "tslen")) < 0) err++;
    if ((mass2_time_dimid = mass2CheckDim(ncid, "time")) < 0) err++;

    if ((mass2_etamax_varid = mass2CheckVar(ncid, "etamax")) < 0) err++;
    if ((mass2_ximax_varid = mass2CheckVar(ncid, "ximax")) < 0) err++;
    if ((mass2_time_varid = mass2CheckVar(ncid, "time")) < 0) err++;
    if ((mass2_x_varid = mass2CheckVar(ncid, "x")) < 0) err++;
    if ((mass2_y_varid = mass2CheckVar(ncid, "y")) < 0) err++;
    if ((mass2_ts_varid = mass2CheckVar(ncid, "timestamp")) < 0) err++;

    if (! err) { 
      mass2ldGetVars(ncid);
      mass2_ncid = ncid;
      strncpy(mass2_file, name, 1024);
    } 
  }

  return (err);
}

/* -------------------------------------------------------------
   mass2CloseFile
   ------------------------------------------------------------- */
int
mass2CloseFile(void)
{
  return (nc_close(mass2_ncid));
}

/* -------------------------------------------------------------
   mass2Times
   ------------------------------------------------------------- */
int
mass2Times(void)
{
  int ncstat = 0;
  size_t n;

  ncstat = nc_inq_dimlen(mass2_ncid, mass2_time_dimid, &n);
  return ((int)n);
}

/* -------------------------------------------------------------
   mass2Blocks
   ------------------------------------------------------------- */
int
mass2Blocks(void)
{
  int ncstat = 0;
  size_t n;

  ncstat = nc_inq_dimlen(mass2_ncid, mass2_blk_dimid, &n);
  return ((int)n);
}

/* -------------------------------------------------------------
   mass2MaxEta
   ------------------------------------------------------------- */
int
mass2MaxEta(void)
{
  int ncstat = 0;
  size_t n;

  ncstat = nc_inq_dimlen(mass2_ncid, mass2_eta_dimid, &n);
  return ((int)n);
}

/* -------------------------------------------------------------
   mass2MaxXi
   ------------------------------------------------------------- */
int
mass2MaxXi(void)
{
  int ncstat = 0;
  size_t n;

  ncstat = nc_inq_dimlen(mass2_ncid, mass2_xi_dimid, &n);
  return ((int)n);
}

/* -------------------------------------------------------------
   mass2BlkEta
   ------------------------------------------------------------- */
int
mass2BlkEta(const int block)
{
  int dims[1];
  size_t index[1];
  int n;
  int ncstat = 0;

  index[0] = block;

  ncstat = nc_get_var1_int(mass2_ncid, mass2_etamax_varid, index, &n);
  return (int)n;
}


/* -------------------------------------------------------------
   mass2BlkXi
   ------------------------------------------------------------- */
int
mass2BlkXi(const int block)
{
  int dims[1];
  size_t index[1];
  int n;
  int ncstat = 0;

  index[0] = block;

  ncstat = nc_get_var1_int(mass2_ncid, mass2_ximax_varid, index, &n);
  return (int)n;
}

/* -------------------------------------------------------------
   mass2GetX
   ------------------------------------------------------------- */
int 
mass2GetX(const int block, double *values)
{
  int eta = mass2BlkEta(block);
  int xi = mass2BlkXi(block);
  size_t start[3];
  size_t length[3];
  int ncstat;

  start[0] = 0;
  start[1] = 0;
  start[2] = block;

  length[0] = xi;
  length[1] = eta;
  length[2] = 1;

  ncstat = nc_get_vara_double(mass2_ncid, mass2_x_varid, start, length, values);
  if (ncstat != NC_NOERR) {
    fprintf(stderr, "mass2GetX: error: %s\n", nc_strerror(ncstat));
  }
  return (ncstat == NC_NOERR);
}  

/* -------------------------------------------------------------
   mass2GetY
   ------------------------------------------------------------- */
int 
mass2GetY(const int block, double *values)
{
  int eta = mass2BlkEta(block);
  int xi = mass2BlkXi(block);
  size_t start[3];
  size_t length[3];
  int ncstat;

  start[0] = 0;
  start[1] = 0;
  start[2] = block;

  length[0] = xi;
  length[1] = eta;
  length[2] = 1;

  ncstat = nc_get_vara_double(mass2_ncid, mass2_y_varid, start, length, values);
  if (ncstat != NC_NOERR) {
    fprintf(stderr, "mass2GetY: error: %s\n", nc_strerror(ncstat));
  }
  return (ncstat == NC_NOERR);
}  

/* -------------------------------------------------------------
   mass2GetVar
   ------------------------------------------------------------- */
int
mass2GetVar(const int timeidx, const int block, 
            const char *varname, float *values)
{
  int varid;
  int eta = mass2BlkEta(block);
  int xi = mass2BlkXi(block);
  int ndims;
  int ncstat;

  size_t start[4];
  size_t length[4];

  if ((ncstat = nc_inq_varid(mass2_ncid, varname, &varid)) != NC_NOERR) {
    return 0;
  }

  ncstat = nc_inq_varndims(mass2_ncid, varid, &ndims);

  switch (ndims) {
  case (3):
    start[0] = 0;
    start[1] = 0;
    start[2] = block;

    length[0] = xi;
    length[1] = eta;
    length[2] = 1;
    break;
  case (4):
    start[0] = timeidx;
    start[1] = 0;
    start[2] = 0;
    start[3] = block;

    length[0] = 1;
    length[1] = xi;
    length[2] = eta;
    length[3] = 1;
    break;
  }


  ncstat = nc_get_vara_float(mass2_ncid, varid, start, length, values);
  if (ncstat != NC_NOERR) {
    fprintf(stderr, "mass2GetVar: error: %s\n", nc_strerror(ncstat));
  }
  return (ncstat == NC_NOERR);
}


/* -------------------------------------------------------------
   mass2TimeStamp
   ------------------------------------------------------------- */
const char *
mass2TimeStamp(int tindex)
{
  int ncstat = 0;
  size_t start[2];
  size_t len[2];
  size_t maxt;

  ncstat = nc_inq_dimlen(mass2_ncid, mass2_time_dimid, &maxt);
  if (tindex > maxt) tindex = maxt;
  
  start[1] = 0; 
  start[0] = tindex;
  len[0] = 1;
  ncstat = nc_inq_dimlen(mass2_ncid, mass2_ts_dimid, &(len[1]));

  strcpy(buffer, "Bad News");
  ncstat = nc_get_vara_text(mass2_ncid, mass2_ts_varid, start, len, buffer);
  /*
  if (ncstat != NC_NOERR) {
    fprintf(stderr, "error getting timestamp %d: %s\n", tindex, nc_strerror(ncstat));
  } else {
    fprintf(stderr, "timestamp successfully retrieved: \"%s\"\n", buffer);
  }
  */

  
  return buffer;
}



