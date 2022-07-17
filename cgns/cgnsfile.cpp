// -------------------------------------------------------------
/**
 * @file   cgnsfile.cpp
 * @author William A. Perkins
 * @date Fri Jun 29 09:35:58 2012
 * 
 * @brief  Implementation of CGNS file I/O classes.
 * 
 * 
 */
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created May 18, 2004 by William A. Perkins
// Last Change: 2018-04-24 11:24:24 d3g096
// -------------------------------------------------------------

static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <cstdlib>
#include <cstring>
#include <vector>
#include <sstream>
#include "cgnsfile.h"

static std::string conndescname("DonorGridConnectivity");



// -------------------------------------------------------------
// -------------------------------------------------------------
// struct CGNSFile::Zone
// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------
// CGNSFile::Zone::vertices
// -------------------------------------------------------------
/** 
 * This routine determines the number of vertices in the zone. The
 * number is determined appropriately whether the zone is structured
 * or unstructured or whether the zone has 1-, 2-, or 3- dimensions.  
 * 
 * 
 * @return number of vertices in zone
 */
int 
CGNSFile::Zone::vertices(void) const
{
  int n, i;
  int dim = base->celldim;

  if (type == cgns::Unstructured) {
    n = size[0][0];
  } else if (type == cgns::Structured) {
    n = 1;
    for (i = 0; i < dim; i++) {
      n *= size[0][i];
    }
  } else {
    throw CGNSFile::error("CGNSFile::Zone::vertices: unknown zone type");
  }
  return n;
}

// -------------------------------------------------------------
// CGNSFile::Zone::cells
// -------------------------------------------------------------
/** 
 * This routine determines, and returns, the total number of cells in
 * the zone.  It only works with "Structured" and "Unstructured" zone
 * types.  Other types are not understood.
 * 
 * 
 * @return total number of cells in the zone
 */
int 
CGNSFile::Zone::cells(void) const
{
  int n, i;
  int dim = base->celldim;

  switch (type) {
  case (cgns::Unstructured):
    n = size[0][1];
    break;
  case (cgns::Structured) :
    n = 1;
    for (i = 0; i < dim; i++) {
      n *= size[1][i];
    }
    break;
  default:
    throw CGNSFile::error("CGNSFile::Zone::cells: unknown zone type");
  }
  return n;
}
  
// -------------------------------------------------------------
// CGNSFile::Zone::setsize
// -------------------------------------------------------------
/** 
 * This routine initializes the size array with the array specified.
 * Only the necessary size values are set, depending on the zone type.
 * 
 * @param s 
 */
void
CGNSFile::Zone::setsize(const cgns::cgsize_t* s)
{
  switch (this->type) {
  case cgns::Structured:
    switch (this->base->celldim) {
    case 1:
      this->size[0][0] = s[0];
      this->size[0][1] = s[1];
      this->size[0][2] = s[2];
      break;
    case 2:
      this->size[0][0] = s[0];
      this->size[0][1] = s[1];
      this->size[1][0] = s[2];
      this->size[1][1] = s[3];
      this->size[2][0] = s[4];
      this->size[2][1] = s[5];
      break;
    case 3:
      this->size[0][0] = s[0];
      this->size[0][1] = s[1];
      this->size[0][2] = s[2];
      this->size[1][0] = s[3];
      this->size[1][1] = s[4];
      this->size[1][2] = s[5];
      this->size[2][0] = s[6];
      this->size[2][1] = s[7];
      this->size[2][2] = s[8];
      break;
    default:
     throw CGNSFile::error("CGNSFile::Zone::setsize: base cell dimension out of range");
      break;
    }
    break;
  case cgns::Unstructured:
    this->size[0][0] = s[0];
    this->size[0][1] = s[1];
    this->size[0][2] = s[2];
    break;
  default:
    throw CGNSFile::error("CGNSFile::Zone::setsize: unknown zone type");
    break;
  }
}

// -------------------------------------------------------------
// -------------------------------------------------------------
// class CGNSFile::Array
// -------------------------------------------------------------
// -------------------------------------------------------------

// -------------------------------------------------------------
// CGNSFile::Array::size
// -------------------------------------------------------------
/** 
 * This routine determines the (total) size of the Array.  This can be
 * used to appropriately size arrays for when the array is read with
 * CGNSFile::arrayread().
 * 
 * 
 * @return total array size
 */
cgns::cgsize_t
CGNSFile::Array::size(void) const
{
  cgns::cgsize_t n = 0;
  if (dim > 0) n = dimvector[0];
  if (dim > 1) n *= dimvector[1];
  if (dim > 2) n *= dimvector[2];
  return n;
}
  

// -------------------------------------------------------------
// -------------------------------------------------------------
// class CGNSFile
// -------------------------------------------------------------
// -------------------------------------------------------------


// -------------------------------------------------------------
// CGNSFile Construction and Destruction
// -------------------------------------------------------------
CGNSFile::CGNSFile(void) 
  : fileidx(-1), myfilename("none"), mymode(-1),
    current_base(), current_zone(), zone_index_cache(), zone_name_cache(),
    my_disable_conn_error(false)
{
}

/** 
 * Given the file name and mode, this constructor will open the CGNS
 * file when the CGNSFile instance is created.
 * 
 * @param name file name
 * @param m mode (default = CG_MODE_READ)
 */
CGNSFile::CGNSFile(const std::string& name, const int& m) 
  : fileidx(-1), myfilename(name), mymode(m), zone_index_cache(), zone_name_cache(),
    my_disable_conn_error(false)
{
  try {
    open(name, m);
  } catch (error& e) {
    throw e;
  }
}

/** 
 * The destructor attempts to close the file.  If the file is not
 * open, an error will occur and this is swallowed.  
 * 
 */
CGNSFile::~CGNSFile(void)
{
  try {
    close();
  } catch (error& e) {
    // swallow any error
  }
}

// -------------------------------------------------------------
// CGNSFile::errormsg
// -------------------------------------------------------------
/** 
 * This routine is used create and throw a CGNSFile::error exception.
 * If ierr is nonzero, an error has occurred, so the specified message
 * is used to create and throw a new CGNSFile::error exception.  If
 * the error code is greater than zero, it is assumed to be a CGNS
 * library error, and the appropriate message is obtained and used.
 * If the code is less than zero, the error is local and only the
 * specified message is used.
 * 
 * @param ierr error code 
 * @param msg error message
 */
void 
CGNSFile::errormsg(const int& ierr, const std::string& msg) const throw(CGNSFile::error)
{
  if (ierr == CG_OK) return;

  const char *emsg = cgns::cg_get_error();
  std::string outmsg;

  if (ierr > 0) {
    outmsg = myfilename + ": " + msg + ": " + emsg;
  } else {
    outmsg = myfilename + ": " + msg;
  }

  throw(error(outmsg, ierr));
}
  
    

// -------------------------------------------------------------
// CGNSFile::open
// -------------------------------------------------------------
/** 
 * This routine attempts to open the specified file using the
 * specified mode (CG_MODE_READ is default).  Mode is one of CG_MODE_READ,
 * CG_MODE_WRITE, CG_MODE_MODIFY.  These are evidently macros in the CGNS
 * header, so they should not have the "cgns::" namespace.  If any
 * CGNS library occurs, a CGNSFile::error exception is thrown.
 * 
 * @param name path of file to open
 * @param m file I/O mode	 
 */
void 
CGNSFile::open(const std::string& name, const int& m)
{
  int ierr;

                                // if the file is already open, we
                                // should close it
  try {
    close();
  } catch (const error& e) { 
    // swallow any error
  }

  myfilename = name;

  ierr = cgns::cg_open(name.c_str(), m, &fileidx);
  try { errormsg(ierr, "cannot open"); } 
  catch (const error& e) { throw (e); }

  mymode = m;

  if (mymode == CG_MODE_WRITE) {
    return;
  }

                                // initialize the base name/id lookup
                                // tables
  try {
    base(1);
    if (zones() > 0) { 
      try { zone(1); } catch (const error& e) { throw (e); }
    }
  } catch (const error& e) { 
    throw (e); 
  }
}

// -------------------------------------------------------------
// CGNSFile::close
// -------------------------------------------------------------
/** 
 * This routine attempts to close the current CGNS file.  If any CGNS
 * library error occurs, a CGNSFile::error exception is thrown.
 * 
 */
void
CGNSFile::close(void)
{
  int ierr;

  ierr = cgns::cg_close(fileidx); 
  try { errormsg(ierr, "cannot close"); } 
  catch (const error& e) { throw (e); }
  mymode = -1;
  zone_index_cache.clear();
  zone_name_cache.clear();
}

// -------------------------------------------------------------
// CGNSFile::bases
// -------------------------------------------------------------
/** 
 * This routine determines the number of base nodes in this CGNS file.
 * 
 * 
 * @return number of base nodes
 */
int
CGNSFile::bases(void) const
{
  int ierr, nb;

  ierr = cgns::cg_nbases(fileidx, &nb);
  try { errormsg(ierr, "cannot find the number of base nodes"); } 
  catch (const error& e) { throw (e); }
  return (nb);
}

// -------------------------------------------------------------
// CGNSFile::baseread
// -------------------------------------------------------------
/** 
 * This routine reads the base node, with index i, from the file.  A
 * CGNSFile::Base structure is created and assigned to ::current_base.
 * 
 * @param i base node index
 * 
 * @return smart pointer to base node information
 */
const CGNSFile::BasePtr&
CGNSFile::baseread(const int& i)
{
  int ierr;
  char buf[1024];
  int cdim, pdim;



  ierr = cgns::cg_base_read(fileidx, i, buf, &cdim, &pdim);
  try { errormsg(ierr, "cannot read base node"); } 
  catch (const error& e) { throw (e); }

  current_base.reset(new Base);
  current_base->index = i;
  current_base->name = buf;
  current_base->celldim = cdim;
  current_base->physdim = pdim;

  current_zone.reset();

  return current_base;
}

// -------------------------------------------------------------
// CGNSFile::base
// -------------------------------------------------------------
/** 
 * The base node indicated is made current.  The new current base is
 * set, then zone information is read from the base node and saved.
 * 
 * @param b index of base node to make current
 */
const CGNSFile::BasePtr&
CGNSFile::base(const int& b)
{
  std::map<int, BasePtr>::const_iterator bid;
  try { 
    switch (mymode) {
    case (CG_MODE_WRITE):
      bid = base_index_cache.find(b);
      if (bid != base_index_cache.end()) {
        current_base = bid->second;
        current_zone.reset();
      } else {
        std::ostringstream msg;
        msg << "Base " << b << " not yet written" << std::ends;
        throw error(msg.str());
      }
      break;
    default:
      baseread(b);
      break;
    }
  } catch (const error& e) { throw e; }
  return current_base;
}

const CGNSFile::BasePtr&
CGNSFile::base(const std::string& s)
{ 
  int i;
  std::map<std::string, BasePtr>::const_iterator bid;

  try {

    switch (mymode) {
    case (CG_MODE_WRITE):
      bid = base_name_cache.find(s);
      if (bid != base_name_cache.end()) {
        current_base = bid->second;
        current_zone.reset();
      } else {
        std::ostringstream msg;
        msg << "Base " << s << " not yet written" << std::ends;
        throw error(msg.str());
      }
      break;
    default:
      for (i = 1; i <= bases(); i++) {
        base(i);
        if (current_base->name == s) {
          current_zone.reset();
          return current_base;
        }
      }
      
      std::string msg;
      msg = "Base \"" + s + "\" not found by name";

      throw CGNSFile::error(msg);
      break;
    }
  } catch (const error& e) { throw e; }

  return current_base;
}


// -------------------------------------------------------------
// CGNSFile::basewrite
// -------------------------------------------------------------
/** 
 * This routine creates a new base node in the CGNS file.  Once
 * created, the base node is made current, and a smart pointer to the
 * base node information is returned.
 * 
 * @param name base node name
 * @param cdim cell dimensionality (3 for volume, 2 for surface)
 * @param pdim physical dimensionality
 * 
 * @return 
 */
const CGNSFile::BasePtr&
CGNSFile::basewrite(const char *name, const int& cdim, const int& pdim)
{
  int ierr;

  current_base.reset(new Base);
  current_base->name = name;
  current_base->celldim = cdim;
  current_base->physdim = pdim;

  ierr = cgns::cg_base_write(fileidx, name, cdim, pdim, 
                             &(current_base->index));
  try {
    errormsg(ierr, "basewrite: cannot write base");
  } catch (const error& e) { throw e; }

  switch (mymode) {
  case (CG_MODE_WRITE):
    base_index_cache.insert(std::make_pair(current_base->index, current_base));
    base_name_cache.insert(std::make_pair(name, current_base));
    break;
  default:
    break;
  }

  current_zone.reset();

  return current_base;
}

// -------------------------------------------------------------
// -------------------------------------------------------------
// Manipulate Miscellaneous Data Structures
// -------------------------------------------------------------
// -------------------------------------------------------------

// -------------------------------------------------------------
// CGNSFile::simulationtyperead
// -------------------------------------------------------------
/** 
 * 
 * 
 * 
 * @return simulation type 
 */
cgns::SimulationType_t 
CGNSFile::simulationtyperead(void)
{
  int ierr;
  cgns::SimulationType_t t;

  try {
    ierr = cgns::cg_simulation_type_read(fileidx, current_base->index, &t);
    errormsg(ierr, "simulationtyperead: cannot read simulation type");
  } catch (const error& e) { throw e; }

  return t;
}

// -------------------------------------------------------------
// CGNSFile::simulationtypewrite
// -------------------------------------------------------------
/** 
 * This routine writes the simulation type, \c TimeAccurate, \c
 * NonTimeAccurate, etc. in the current base node.
 * 
 * @param t simulation type
 */
void 
CGNSFile::simulationtypewrite(const cgns::SimulationType_t& t)
{
  int ierr;
  try {
    ierr = cgns::cg_simulation_type_write(fileidx, current_base->index, t);
    errormsg(ierr, "simulationtypewrite: cannot write simulation type");
  } catch (const error& e) { throw e; }
}


// -------------------------------------------------------------
// CGNSFile::gravityread
// -------------------------------------------------------------
/** 
 * The gravity vector is assumed to have the same physically
 * dimensionality the current base node (CGNSFile::Base::physdim).
 * 
 * @param g array to put gravity vector components in
 */
void
CGNSFile::gravityread(double *g)
{
  int ierr;
  float gin[current_base->physdim];

  ierr = cgns::cg_gravity_read(fileidx, current_base->index, gin);

  try {
    errormsg(ierr, "gravityread: cannot read gravity");
  } catch (const error& e) { throw e; }

  for (int i = 0; i < current_base->physdim; i++) {
    g[i] = (double)gin[i];
  }
}
   

// -------------------------------------------------------------
// CGNSFile::gravitywrite
// -------------------------------------------------------------
/** 
 * The gravity vector is assumed to have the same physically
 * dimensionality the current base node (CGNSFile::Base::physdim).
 * 
 * @param g 
 */
void
CGNSFile::gravitywrite(const double *g)
{
  int ierr;

  if (!static_cast<bool>(current_base)) {
    errormsg(ierr, "gravitywrite: current base not set");
  }

  float gout[current_base->physdim];

  for (int i = 0; i < current_base->physdim; i++) {
    gout[i] = g[i];
  }

  try {
    ierr = cgns::cg_gravity_write(fileidx, current_base->index, &gout[0]);
    errormsg(ierr, "gravitywrite: cannot write gravity");
  } catch (const error& e) { throw e; }
}


// -------------------------------------------------------------
// CGNSFile::zones
// -------------------------------------------------------------
/** 
 * 
 * 
 * 
 * @return number of zones
 */
int
CGNSFile::zones() const
{
  int ierr, nz;

  try {
    errormsg((!(bool)current_base), "no current base node");
    ierr = cgns::cg_nzones(fileidx, current_base->index, &nz);
    errormsg(ierr, "cannot read number of zones");
  } catch (const error& e) { throw e; }

  return nz;
}


// -------------------------------------------------------------
// CGNSFile::zoneread
// -------------------------------------------------------------
/** 
 * This routine reads the zone from the current base.  This tries to
 * hide the zone size. 
 * 
 * @param idx zone index
 * 
 * @return smart pointer to the current zone
 */
const CGNSFile::ZonePtr& 
CGNSFile::zoneread(const int& idx)
{
  int ierr;
  char buf[1024];
  cgns::cgsize_t size[9];

                                // initialize this before sending it
                                // to the library
  for (int i = 0; i < 9; i++) {
    size[i] = 0;
  }

  try {

                                // make a new current zone

    current_zone.reset(new Zone(current_base));
    current_zone->index = idx;

                                // use zone type to determine
                                // cell/vertex size dimensions

    ierr = cgns::cg_zone_type(fileidx, current_base->index, current_zone->index,
                              &(current_zone->type));
    errormsg(ierr, "cannot read zone type");

    errormsg((!(bool)current_base), "no current base node");
    ierr = cgns::cg_zone_read(fileidx, current_base->index, idx, 
                            buf, &(size[0]));
    errormsg(ierr, "cannot read zone");

    current_zone->name = buf;

    current_zone->setsize(size);

  } catch (const error& e) { throw e; }

  return current_zone;
}

// -------------------------------------------------------------
// CGNSFile::zone
// -------------------------------------------------------------
/** 
 * Makes the zone with index i the current zone.  
 * 
 * @param i zone index
 * 
 * @return smart pointer to the current zone
 */
const CGNSFile::ZonePtr&
CGNSFile::zone(const int& i)
{
  std::map<int, ZonePtr>::const_iterator zid;
  try { 
    switch (mymode) {
    case (CG_MODE_WRITE):
      zid = zone_index_cache.find(i);
      if (zid != zone_index_cache.end()) {
        current_zone = zid->second;
        current_base = current_zone->base;
      } else {
        std::ostringstream msg;
        msg << "Zone " << i << " not yet written" << std::ends;
        throw error(msg.str());
      }
      break;
    default:
      zoneread(i);
      break;
    }
  } catch (const error& e) { throw e; }
  return current_zone;
}

const CGNSFile::ZonePtr&
CGNSFile::zone(const std::string& s)
{
  int i;
  ZonePtr oldz = current_zone;
  std::map<std::string, ZonePtr>::const_iterator zid;

  try {

    switch (mymode) {
    case (CG_MODE_WRITE):
      zid = zone_name_cache.find(s);
      if (zid != zone_name_cache.end()) {
        current_zone = zid->second;
        current_base = current_zone->base;
      } else {
        std::ostringstream msg;
        msg << "Zone " << s << " not yet written" << std::ends;
        throw error(msg.str());
      }
      break;
    default:
      for (i = 1; i <= zones(); i++) {
        zone(i);
        if (zone()->name == s) {
          return current_zone;
        }
      }
      current_zone.reset();

      std::string msg;
      msg = "Zone \"" + s + "\" not found by name";
      
      throw CGNSFile::error(msg);
      break;
    }
  } catch (const error& e) { throw e; }

  return current_zone;
}

const CGNSFile::ZonePtr&
CGNSFile::zone(void) const
{
  return current_zone;
}


// -------------------------------------------------------------
// CGNSFile::zonewrite
// -------------------------------------------------------------
/** 
 * 
 * 
 * @param name name of new zone
 * @param size zone sizes
 * @param type type of new zone
 * 
 * @return smart pointer to current zone
 */
const CGNSFile::ZonePtr&
CGNSFile::zonewrite(const char *name, cgns::cgsize_t *size, const cgns::ZoneType_t& type)
{
  int ierr(0), id;

  

  try {
    errormsg((!(bool)current_base), "no current base node");
    ierr = cgns::cg_zone_write(fileidx, current_base->index, 
                               name, size, type, &id);
  } catch (const error& e) { throw e; }

  current_zone.reset(new Zone(current_base));
  current_zone->index = id;
  current_zone->type = type;
  current_zone->name = name;
  current_zone->setsize(size);

  switch (mymode) {
  case (CG_MODE_WRITE):
    zone_index_cache.insert(std::make_pair(id, current_zone));
    zone_name_cache.insert(std::make_pair(name, current_zone));
    break;
  default:
    break;
  }

  return current_zone;
}


// -------------------------------------------------------------
// CGNSFile::cells
// -------------------------------------------------------------
int
CGNSFile::cells(void) 
{
  int ncell = 0;
  for (int z = 1; z <= zones(); z++) {
    ncell += zone(z)->cells();
  }
  return ncell;
}

// -------------------------------------------------------------
// CGNSFile::vertices
// -------------------------------------------------------------
int
CGNSFile::vertices(void)
{
  int nvert = 0;
  for (int z = 1; z <= zones(); z++) {
    nvert += zone(z)->vertices();
  }
  return nvert;
}

// -------------------------------------------------------------
// CGNSFile::grids
// -------------------------------------------------------------
/** 
 * 
 * 
 * 
 * @return number of grids in the current zone
 */
int
CGNSFile::grids(void) const
{
  int ierr, n;

  try {
    ierr = cgns::cg_ngrids(fileidx, current_base->index, current_zone->index, &n);
    errormsg(ierr, "cannont read number of grids");
  } catch (const error& e) { throw e; }

  return (n);
}

// -------------------------------------------------------------
// CGNSFile::gridindex
// -------------------------------------------------------------
int
CGNSFile::gridindex(const char *gridname) const
{
  const char *gn = gridname;
  std::string gname;
  int ng, g;

  if (gn == NULL) gn = "GridCoordinates";

  try {
    ng = grids();
    for (g = 1; g <= ng; g++) {
      gname = gridread(g);
      if (gname == gn) break;
    }
    gname = gn;
    gname += ": grid not found";
    errormsg((g < ng), gname.c_str());

  } catch (const error& e) { throw e; }
  return g;
}

  
// -------------------------------------------------------------
// CGNSFile::gridread
// -------------------------------------------------------------
/** 
 * 
 * 
 * @param i index of grid node to read
 * 
 * @return name of grid node
 */
std::string
CGNSFile::gridread(const int& i) const
{
  int ierr;
  std::string name;
  char buf[1024];

  try {
    ierr = cgns::cg_grid_read(fileidx, current_base->index, 
                              current_zone->index, i, buf);
    errormsg(ierr, "cannot read grid");
  } catch (const error& e) { throw e; }
  name = buf;

  return (name);
}


// -------------------------------------------------------------
// CGNSFile::gridwrite
/** 
 * 
 * 
 * @param name name of grid node to write
 * 
 * @return index of new grid node.
 */
int
CGNSFile::gridwrite(const char *name)
{
  int ierr, id;
  
  try {
    ierr = cgns::cg_grid_write(fileidx, current_base->index, 
                               current_zone->index, name, &id);
    errormsg(ierr, "cannot write grid");
  } catch (const error& e) { throw e; }
  return id;    
}


// -------------------------------------------------------------
// CGNSFile::narrays
// -------------------------------------------------------------
/** 
 * 
 * 
 * 
 * @return number of data arrays in current node
 */
int 
CGNSFile::narrays(void) const
{
  int n = 0, ierr;
  try {
    ierr = cgns::cg_narrays(&n);
    errormsg(ierr, "cannot get number of data arrays");
  } catch (const error& e) { throw e; }
  return n;    
}

// -------------------------------------------------------------
// CGNSFile::arrayinfo
/** 
 * This routine gets information for an array in the current node.
 * 
 * @param i array index
 *
 * @return shared pointer to array information
 */
// -------------------------------------------------------------
CGNSFile::ArrayPtr 
CGNSFile::arrayinfo(const int& i) const
{
  int ierr;
  char buf[1024];
  CGNSFile::ArrayPtr a(new Array());

  a->index = i;
  try {
    ierr = cgns::cg_array_info(a->index, buf, &(a->datatype), &(a->dim), a->dimvector);
    errormsg(ierr, "cannot read array info");
  } catch (const error& e) { throw e; }
  a->name = buf;

  return (a);
}

// -------------------------------------------------------------
// CGNSFile::arrayread
// -------------------------------------------------------------
/** 
 * 
 * 
 * @param i data array index
 * @param data pointer to a chunk of memory to store the data
 * @param astype type 
 */
void
CGNSFile::arrayread(const int& i, void *data, cgns::DataType_t astype) const
{
  int ierr;

  try {
    ierr = cgns::cg_array_read_as(i, astype, data);
    errormsg(ierr, "cannot read array");
  } catch (const error& e) { throw e; }
                                  
}

// -------------------------------------------------------------
// CGNSFile::arraywrite
// -------------------------------------------------------------
void
CGNSFile::arraywrite(const char* name, const cgns::DataType_t& type, 
                     const int& dims, const cgns::cgsize_t *dimlen, const void *data)
{
  try {
    int ierr = cgns::cg_array_write(name, type, dims, dimlen, data);
    errormsg(ierr, "cannot write array");
  } catch (const error& e) { throw e; }
}  

// -------------------------------------------------------------
// CGNSFile::ncoords
// -------------------------------------------------------------
/** 
 * The name of the grid is optional and defaults to the SIDS default
 * "GridCoordinates".  
 * 
 * @param gridname name of grid node (optional)
 * 
 * @return number of coordinates in grid node
 */
int 
CGNSFile::ncoords(const char *gridname) const
{
  int ierr, nc, g;
  std::string gname;

  try {

    g = gridindex(gridname);    // find an index for the named grid

                                // go to that grid node
    ierr = cgns::cg_goto(fileidx, current_base->index, 
                         "Zone_t", current_zone->index, 
                         "GridCoordinates_t", g, "end");
    errormsg(ierr, "cannot go to grid node");

                                // count the number of data arrays in
                                // the grid
    ierr = cgns::cg_narrays(&nc);
    errormsg(ierr, "cannot read number of coord arrays");
  } catch (const error& e) { throw e; }

  return nc;
}

// -------------------------------------------------------------
// CGNSFile::coordinfo
// -------------------------------------------------------------
/** 
 * This routine reads information about the specified coordinate in
 * the current zone.  By default, the grid named "GridCoordinates" is
 * read.  The coordinate is specified by index.  The data type is not
 * returned, since it will be converted to double no matter what it
 * is.
 * 
 * @param i coordinate index
 * @param gridname grid name (default: "GridCoordinates")
 * 
 * @return coordinate name
 */
CGNSFile::ArrayPtr
CGNSFile::coordinfo(const int& i, const char *gridname)
{
  int ierr;
  int g;
  ArrayPtr a;

  try {
    g = gridindex(gridname);    // find an index for the named grid

                                // go to that grid node
    ierr = cgns::cg_goto(fileidx, current_base->index, 
                         "Zone_t", current_zone->index, 
                         "GridCoordinates_t", g, "end");
    errormsg(ierr, "cannot go to grid node");

    a = arrayinfo(i);

                                // we probably should check to make
                                // sure the dimensions are correct for
                                // the current zone

  } catch (const error& e) { throw e; }
  return a;
}


// -------------------------------------------------------------
// CGNSFile::coordread
// -------------------------------------------------------------
/** 
 * This reads a coordinate from the specified grid in the current
 * zone.  The default grid has the name "GridCoordinates".  The
 * coordinate is specified by name.  The coordinate values (regardless
 * of the type in the file) are placed in the chunk of memory pointed
 * to by coord (it is assumed to be large enough).  
 * 
 * @param coordname name of coordinate variable to read
 * @param coord array to put coordinate values in
 * @param gridname (optional) name of grid from which to read
 * 
 * @return index of coordinate read
 */
int
CGNSFile::coordread(const char *coordname, 
                    double *coord, const char *gridname)
{
  int ierr;
  int c, g;
  int nc;
  std::string msg;

  try {
    nc = ncoords(gridname);
    for (c = 1; c <= nc; c++) {
      ArrayPtr a(coordinfo(c, gridname));
      if (a->name == coordname) break;
    }
    if (c > nc) {
      msg = "cannot find coordinate \"" ;
      msg += coordname;
      msg += "\"";
      errormsg(0, msg.c_str());
    }
    g = gridindex(gridname);
    ierr = cgns::cg_goto(fileidx, current_base->index, 
                         "Zone_t", current_zone->index, 
                         "GridCoordinates_t", g, "end");
    errormsg(ierr, "cannot go to grid node");

    arrayread(c, (void *)coord, cgns::RealDouble);

  } catch (const error& e) { throw e; }
  return c;
}

// -------------------------------------------------------------
// CGNSFile::coordread
// -------------------------------------------------------------
void
CGNSFile::coordread(const int& i, double *coord, const char *gridname)
{
  int ierr;
  int g;
  std::string msg;

  try {
    g = gridindex(gridname);
    ierr = cgns::cg_goto(fileidx, current_base->index, 
                         "Zone_t", current_zone->index, 
                         "GridCoordinates_t", g, "end");
    errormsg(ierr, "cannot go to grid node");

    arrayread(i, (void *)coord, cgns::RealDouble);

  } catch (const error& e) { throw e; }
  return;
}

// -------------------------------------------------------------
// CGNSFile::coordwrite
// -------------------------------------------------------------
void
CGNSFile::coordwrite(const char *coordname, 
                     const void *coord, 
                     const int& gidx,
                     const cgns::DataType_t coordtype)
{
  int ierr, id;
  try {
    if (!(bool)current_zone) errormsg(1, "no current zone");
    if (gidx == 0) {
      ierr = cg_coord_write(fileidx, 
                            current_zone->base->index,
                            current_zone->index, coordtype,
                            coordname, coord, &id);
      errormsg(ierr, "cannot write coordinate");
    } else {
      ierr = cgns::cg_goto(fileidx, current_base->index, 
                           "Zone_t", current_zone->index, 
                           "GridCoordinates_t", gidx, "end");
      errormsg(ierr, "cannot go to grid node");

      int nlen;
      cgns::cgsize_t len[3];

      switch (current_zone->type) {
      case cgns::Structured:
        nlen = current_zone->base->celldim;
        len[0] = current_zone->size[0][0];
        if (nlen > 1) len[1] = current_zone->size[0][1];
        if (nlen > 2) len[2] = current_zone->size[0][2];
        break;
      case cgns::Unstructured:
        nlen = 1;
        len[0] = current_zone->size[0][0];
        break;
      default:
        errormsg(1, "unknown zone type in coordwrite");
        break;
      }
        
      arraywrite(coordname, coordtype, nlen, len, coord);
    }
  } catch (const error& e) { throw e; }
}



// -------------------------------------------------------------
// CGNSFile::npe
// -------------------------------------------------------------
/** 
 * Get the number of vertices for the given element type.  
 *
 * @todo should be static
 * 
 * @param t element type
 * 
 * @return number of vertices per element
 */
int 
CGNSFile::npe(const cgns::ElementType_t& t)
{
  int ierr = 0;
  int n;
  try {
    ierr = cgns::cg_npe(t, &n);
    errormsg(ierr, "cannot get nodes per element");
  } catch (const error& e) { throw e; }
  return n;
}

// -------------------------------------------------------------
// CGNSFile::nsections
// -------------------------------------------------------------
/** 
 * 
 * 
 * 
 * @return sections in current zone
 */
int
CGNSFile::nsections(void)
{
  int ierr = 0;
  int n;
  try {
    ierr = cgns::cg_nsections(fileidx, current_base->index, current_zone->index, &n);
    errormsg(ierr, "cannot read number of sections");
  } catch (const error& e) { throw e; }
  return n;
}

// -------------------------------------------------------------
// CGNSFile::newsection
// -------------------------------------------------------------
CGNSFile::SectionPtr
CGNSFile::newsection(void)
{
  SectionPtr s(new Section(current_zone));
  return (s);
}

// -------------------------------------------------------------
// CGNSFile::sectionread
// -------------------------------------------------------------
CGNSFile::SectionPtr
CGNSFile::sectionread(const int& i)
{
  int ierr;
  SectionPtr s(newsection());
  char name[1024];
  cgns::ElementType_t type;
  cgns::cgsize_t start, end;
  int nbndry, parent;

  s->index = i;
  
  try {
    ierr = cgns::cg_section_read(fileidx, current_base->index, 
                                 current_zone->index, s->index,
                                 name, &type, &start, &end, &nbndry, &parent);
    errormsg(ierr, "cannot read section");
  } catch (const error& e) { throw e; }

  s->name = name;
  s->type = type;
  s->idxbeg = start;
  s->idxend = end;
  s->nbnd = nbndry;
  s->parent = (parent == 1);

  return (s);
}
                                 
// -------------------------------------------------------------
// CGNSFile::sectionwrite
// -------------------------------------------------------------
/** 
 * 
 * 
 * @param s 
 * @param e 
 * @param p 
 */
void
CGNSFile::sectionwrite(CGNSFile::SectionPtr& s, cgns::cgsize_t *e, cgns::cgsize_t *p)
{
  int ierr;

  s->parent = (p != NULL);

  try {
    ierr = cgns::cg_section_write(fileidx, s->zone->base->index, s->zone->index,
                                  s->name.c_str(), s->type, s->idxbeg, s->idxend,
                                  s->nbnd, e, &(s->index));
    errormsg(ierr, "cannot write section");
    if (s->parent) {
      ierr = cgns::cg_parent_data_write(fileidx, s->zone->base->index, 
                                        s->zone->index, s->index, p);
      errormsg(ierr, "cannot write section parent data");
    }
  } catch (const error& e) { throw e; }
}

// -------------------------------------------------------------
// CGNSFile::elementdatasize
// -------------------------------------------------------------
/** 
 * 
 * 
 * @param s 
 * 
 * @return 
 */
int 
CGNSFile::elementdatasize(const CGNSFile::SectionPtr& s)
{
  int ierr;
  cgns::cgsize_t n;
  try {
    ierr = cgns::cg_ElementDataSize(fileidx, s->zone->base->index, 
                                    s->zone->index, s->index, &n);
    errormsg(ierr, "cannot get element data size");
  } catch (const error& e) { throw e; }
  return n;
}

// -------------------------------------------------------------
// CGNSFile::elementsread
// -------------------------------------------------------------
/** 
 * Must call ::sectionread to get Section information.  Use
 * ::elementdatasize to determine the size of memory chunks pointed to
 * by e and p; p is 4 times larger than e.
 * 
 * @param s 
 * @param e 
 * @param p 
 */
void
CGNSFile::elementsread(const SectionPtr& s, cgns::cgsize_t *e, cgns::cgsize_t *p)
{
  int ierr;

  try {
    ierr = cgns::cg_elements_read(fileidx, s->zone->base->index, 
                                  s->zone->index, s->index, e, p);
    errormsg(ierr, "cannot read elements");
  } catch (const error& e) { throw e; }
}

// -------------------------------------------------------------
// CGNSFile::nbc
// -------------------------------------------------------------
/** 
 * 
 * 
 * 
 * @return number of boundary conditions in current zone
 */
int
CGNSFile::nbc(void)
{
  int ierr, n;
  try {
    ierr = cgns::cg_nbocos(fileidx, current_base->index, current_zone->index, &n);
    errormsg(ierr, "cannot read number of BCs");
  } catch (const error& e) { throw e; }
  return n;
}


// -------------------------------------------------------------
// CGNSFile::bcinfo
// -------------------------------------------------------------
CGNSFile::BCPtr
CGNSFile::bcinfo(const int& i) const
{
  int ierr;
  BCPtr bc(new BC(current_zone));
  char buf[1024];
  std::string msg;
  
  bc->index = i;
  bc->location = cgns::Vertex;
  try {
    ierr = cgns::cg_boco_info(fileidx, bc->zone->base->index, bc->zone->index,
                              bc->index, buf, &bc->type, &bc->pttype, &bc->npts, 
                              bc->normidx, &bc->normidxflag, &bc->normtype, &bc->ndatasets);
    errormsg(ierr, "cannot read BC info");
    bc->name = buf;
    ierr = cgns::cg_goto(fileidx, bc->zone->base->index, 
                         "Zone_t", bc->zone->index, 
                         "ZoneBC_t", 1, "BC_t", bc->index, "end");
    if (ierr == 0) {
      ierr = cg_gridlocation_read(&bc->location);
      // ignore error
    }

    if (bc->type == cgns::FamilySpecified) {
      ierr = cgns::cg_famname_read(buf);
      std::string s(buf);

      if (ierr != CG_OK) {
        msg = bc->name;
        msg += ": BC type is \"FamilySpecified\", but cannot read Family name";
        errormsg(ierr, msg);
      }

      int nfam;
      ierr = cgns::cg_nfamilies(fileidx, bc->zone->base->index, &nfam);
      errormsg(ierr, "cannot read number of Familys");

      if ((nfam <= 0)) 
        errormsg(CG_ERROR, "need Family specs, but none available");

      for (int f = 1; i <= nfam; ++f) {
        int nbc, ngeo;
        ierr = cgns::cg_family_read(fileidx, bc->zone->base->index, f,
                                    buf, &nbc, &ngeo);
        errormsg(ierr, "cannot read Family");

        if (s == buf && nbc > 0) {
          char nbuf[1024];
          cgns::BCType_t fbctype;
          ierr = cgns::cg_fambc_read(fileidx, bc->zone->base->index, 
                                     f, 1, nbuf, &fbctype);
          errormsg(ierr, "cannot read Family BC");
          bc->type = fbctype;
          break;
        }
      }
      if (bc->type == cgns::FamilySpecified)
        errormsg(CG_ERROR, 
                 "unable to resolve \"FamilySpecified\" BC type");
    }
  } catch (const error& e) { throw e; }
  return bc;
}

// -------------------------------------------------------------
// CGNSFile::bcread
// -------------------------------------------------------------
/** 
 * Before calling this, bcinfo() should be called to determine the
 * size of the integer array needed to store the point list.
 * 
 * @param bc index of boundary condition in current zone
 * @param pts memory to store boundary point data
 */
void 
CGNSFile::bcread(const int& bc, cgns::cgsize_t *pts) const
{
  int ierr;
  try {
    ierr = cgns::cg_boco_read(fileidx, current_zone->base->index, 
                              current_zone->index, bc, pts, NULL);
    errormsg(ierr, "cannot read boundary condition");
  } catch (const error& e) { throw e; }
}

// -------------------------------------------------------------
// CGNSFile::newbc
// -------------------------------------------------------------
CGNSFile::BCPtr
CGNSFile::newbc(void) const
{
  BCPtr bc(new BC(current_zone));
  return (bc);
}

// -------------------------------------------------------------
// CGNSFile::bcwrite
// -------------------------------------------------------------
void
CGNSFile::bcwrite(BCPtr& bp, const cgns::cgsize_t *pts)
{
  int ierr;

  try {
    ierr = cgns::cg_boco_write(fileidx, bp->zone->base->index, 
                               bp->zone->index, bp->name.c_str(), 
                               bp->type, bp->pttype, bp->npts, pts, 
                               &bp->index);
    errormsg(ierr, "cannot write boundary condition");
  } catch (const error& e) { throw e; }
}


// -------------------------------------------------------------
// -------------------------------------------------------------
// Manipulation of Discrete Data nodes
// -------------------------------------------------------------
// -------------------------------------------------------------

// -------------------------------------------------------------
// CGNSFile::ndiscrete
// -------------------------------------------------------------
/** 
 * 
 * 
 * 
 * @return number of DiscreteData nodes in the current zone
 */
int 
CGNSFile::ndiscrete(void) const
{
  int ierr = 0;
  int nd;
  try { 
    ierr = cgns::cg_ndiscrete(fileidx, current_zone->base->index,
                              current_zone->index, &nd);
    errormsg(ierr, "cannot read number of discrete data");
  } catch (const error& e) { throw e; }
  return nd;
}



// -------------------------------------------------------------
// CGNSFile::discreteread
// -------------------------------------------------------------
void
CGNSFile::discreteread(const int& n, std::string& name, cgns::GridLocation_t& outloc) const
{
  int ierr;
  char buf[1024];
  cgns::GridLocation_t loc;
  
  try {
    ierr = cgns::cg_discrete_read(fileidx, current_zone->base->index,
                                  current_zone->index, n, buf);
    errormsg(ierr, "cannot read discrete data node");
    ierr = cgns::cg_goto(fileidx, current_zone->base->index,
                         "Zone_t", current_zone->index,
                         "DiscreteData_t", n, "end");
    ierr = cgns::cg_gridlocation_read(&loc);
  } catch (const error& e) { throw e; }
  name = buf;
  outloc = loc;
}

void
CGNSFile::discreteread(const char* name, int& idx, cgns::GridLocation_t& loc) const
{
  std::string nodename;
  cgns::GridLocation_t nodeloc;
  int ndd;
  bool found = false;
  
  try {
    ndd = ndiscrete();
    for (int i = 1; i <= ndd; i++) {
      discreteread(i, nodename, nodeloc);
      if (nodename == name) {
        found = true;
        idx = i;
        loc = nodeloc;
        break;
      }
    }
    nodename = "cannot find DiscreteData node \"";
    nodename += name;
    nodename += "\"";
    errormsg(!found, nodename);
  } catch (const error& e) { throw e; }
  return;
}
  

// -------------------------------------------------------------
// CGNSFile::discretewrite
// -------------------------------------------------------------
/** 
 * 
 * 
 * @param name Name of DiscreteData node to create
 * @param loc Grid location of data in new DiscreteData node
 * 
 * @return Index of the new DiscreteData node
 */
int
CGNSFile::discretewrite(const char* name, const cgns::GridLocation_t loc)
{
  int ierr = 0;
  int id;

  try {
    ierr = cgns::cg_discrete_write(fileidx, current_zone->base->index, 
                                   current_zone->index, name, &id);
    errormsg(ierr, "cannot write discrete data");
    ierr = cgns::cg_goto(fileidx, current_zone->base->index,
                         "Zone_t", current_zone->index,
                         "DiscreteData_t", id, "end");
    errormsg(ierr, "cannot goto discrete data node");
    ierr = cgns::cg_gridlocation_write(loc);
    errormsg(ierr, "cannot write discrete data grid location");
  } catch (const error& e) { throw e; }
  return id;
}


// -------------------------------------------------------------
// CGNSFile::ndiscretefields
// -------------------------------------------------------------
int 
CGNSFile::ndiscretefields(const int& d) const
{
  int ierr;
  int ndata;
  try {
    ierr = cgns::cg_goto(fileidx, current_zone->base->index,
                         "Zone_t", current_zone->index,
                         "DiscreteData_t", d, "end");
    errormsg(ierr, "cannot goto DiscreteData node");
    ndata = narrays();
  } catch (const error& e) { throw e; }
  return ndata;
}

// -------------------------------------------------------------
// CGNSFile::discretefieldinfo
// -------------------------------------------------------------
void 
CGNSFile::discretefieldinfo(const int& d, const int& fidx, std::string& name, 
                            cgns::DataType_t& ftype) const
{
  ArrayPtr a;
  int ierr;
  try {
    ierr = cgns::cg_goto(fileidx, current_zone->base->index,
                         "Zone_t", current_zone->index,
                         "DiscreteData_t", d, "end");
    errormsg(ierr, "cannot goto DiscreteData node");
    a = arrayinfo(fidx);
  } catch (const error& e) { throw e; }
  name = a->name;
  ftype = a->datatype;
}


// -------------------------------------------------------------
// CGNSFile::discretefieldbyname
// -------------------------------------------------------------
int 
CGNSFile::discretefieldbyname(const int& d, const char *name) const
{
  int fidx;
  int nf;
  ArrayPtr a;
  try {
    nf = ndiscretefields(d);    // does cg_goto
    for (fidx = 1; fidx <= nf; fidx++) {
      a = arrayinfo(fidx);
      if (a->name == name) break;
    }
    if (fidx > nf) {
      std::string msg;
      msg = "\"";
      msg += name;
      msg += "\": discrete data field name not found";
      errormsg(1, msg);
    }
  } catch (const error& e) { throw e; }
  return fidx;
}

// -------------------------------------------------------------
// CGNSFile::discretefieldread
// -------------------------------------------------------------
void
CGNSFile::discretefieldread(const int& d, const char *fname, double *data) const
{
  int fidx;
  try {
    fidx = discretefieldbyname(d, fname); // does cg_goto
    arrayread(fidx, data,cgns::RealDouble);
  } catch (const error& e) { throw e; }
}

// -------------------------------------------------------------
// CGNSFile::discretefieldread
// -------------------------------------------------------------
void
CGNSFile::discretefieldread(const int& d, const char *fname, int *data) const
{
  int fidx;
  try {
    fidx = discretefieldbyname(d, fname); // does cg_goto
    arrayread(fidx, data,cgns::Integer);
  } catch (const error& e) { throw e; }
}

#if CG_BUILD_64BIT
// -------------------------------------------------------------
// CGNSFile::discretefieldread
// -------------------------------------------------------------
void
CGNSFile::discretefieldread(const int& d, const char *fname, cgns::cgsize_t *data) const
{
  int fidx;
  try {
    fidx = discretefieldbyname(d, fname); // does cg_goto
    if (sizeof(cgns::cgsize_t) == sizeof(int)) {
      arrayread(fidx, data,cgns::Integer);
    } else {
      arrayread(fidx, data,cgns::LongInteger);
    }
  } catch (const error& e) { throw e; }
}
#endif

// -------------------------------------------------------------
// CGNSFile::discretefieldwrite
// -------------------------------------------------------------
void
CGNSFile::discretefieldwrite(const int& d, const char *fname, 
                             const cgns::GridLocation_t& loc, const double *data) 
{
  int ierr;
  ZonePtr z;
  try {
    z = zone();
    cgns::cgsize_t dimlen[3];
    int ndim;
    switch (z->type) {
    case (cgns::Unstructured):
      ndim = 1;
      switch (loc) {
      case (cgns::CellCenter):
        dimlen[0] = z->size[0][1];
        break;
      case (cgns::Vertex):
        dimlen[0] = z->size[0][0];
        break;
      default:
        throw error("CGNSFile::discretefieldwrite: Unable to deal with specified grid location");
      }
      break;
    case (cgns::Structured):
      ndim = z->base->celldim;
      switch (loc) {
      case (cgns::CellCenter):
        dimlen[0] = z->size[1][0];
        dimlen[1] = z->size[1][1];
        dimlen[2] = z->size[1][2];
        break;
      case (cgns::Vertex):
        dimlen[0] = z->size[0][0];
        dimlen[1] = z->size[1][0];
        dimlen[2] = z->size[2][0];
        break;
      default:
        throw error("CGNSFile::discretefieldwrite: Unable to deal with specified grid location");
      }
      break;
    default:
      throw error("CGNSFile::discretefieldwrite: Unable to deal with zone type");
    }
    ierr = cgns::cg_goto(fileidx, current_zone->base->index,
                         "Zone_t", current_zone->index,
                         "DiscreteData_t", d, "end");
    errormsg(ierr, "cannot go to discrete data node");
    arraywrite(fname, cgns::RealDouble, ndim, dimlen, data);
  } catch (const error& e) { throw e; }
}

#if CG_BUILD_64BIT
// -------------------------------------------------------------
// CGNSFile::discretefieldwrite
// -------------------------------------------------------------
void
CGNSFile::discretefieldwrite(const int& d, const char *fname, 
                             const cgns::GridLocation_t& loc, const cgns::cgsize_t *data) 
{
  int ierr;
  ZonePtr z;
  try {
    z = zone();
    cgns::cgsize_t dimlen[3] = { 0, 0, 0 };
    int ndim;
    switch (z->type) {
    case (cgns::Unstructured):
      ndim = 1;
      switch (loc) {
      case (cgns::CellCenter):
        dimlen[0] = z->size[0][1];
        break;
      case (cgns::Vertex):
        dimlen[0] = z->size[0][0];
        break;
      default:
        throw error("CGNSFile::discretefieldwrite: Unable to deal with specified grid location");
      }
      break;
    case (cgns::Structured):
      ndim = z->base->celldim;
      switch (loc) {
      case (cgns::CellCenter):
        dimlen[0] = z->size[1][0];
        dimlen[1] = z->size[1][1];
        dimlen[2] = z->size[1][2];
        break;
      case (cgns::Vertex):
        dimlen[0] = z->size[0][0];
        dimlen[1] = z->size[1][0];
        dimlen[2] = z->size[2][0];
        break;
      default:
        throw error("CGNSFile::discretefieldwrite: Unable to deal with specified grid location");
      }
      break;
    default:
      throw error("CGNSFile::discretefieldwrite: Unable to deal with zone type");
    }
    ierr = cgns::cg_goto(fileidx, current_zone->base->index,
                         "Zone_t", current_zone->index,
                         "DiscreteData_t", d, "end");
    errormsg(ierr, "cannot go to discrete data node");
    if (sizeof(cgns::cgsize_t) == sizeof(int)) {
      arraywrite(fname, cgns::Integer, ndim, dimlen, data);
    } else {
      arraywrite(fname, cgns::LongInteger, ndim, dimlen, data);
    }
  } catch (const error& e) { throw e; }
}
#endif

// -------------------------------------------------------------
// CGNSFile::discretefieldwrite
// -------------------------------------------------------------
void
CGNSFile::discretefieldwrite(const int& d, const char *fname, 
                             const cgns::GridLocation_t& loc, const int *data) 
{
  int ierr;
  ZonePtr z;
  try {
    z = zone();
    cgns::cgsize_t dimlen[3] = { 0, 0, 0 };
    int ndim;
    switch (z->type) {
    case (cgns::Unstructured):
      ndim = 1;
      switch (loc) {
      case (cgns::CellCenter):
        dimlen[0] = z->size[0][1];
        break;
      case (cgns::Vertex):
        dimlen[0] = z->size[0][0];
        break;
      default:
        throw error("CGNSFile::discretefieldwrite: Unable to deal with specified grid location");
      }
      break;
    case (cgns::Structured):
      ndim = z->base->celldim;
      switch (loc) {
      case (cgns::CellCenter):
        dimlen[0] = z->size[1][0];
        dimlen[1] = z->size[1][1];
        dimlen[2] = z->size[1][2];
        break;
      case (cgns::Vertex):
        dimlen[0] = z->size[0][0];
        dimlen[1] = z->size[1][0];
        dimlen[2] = z->size[2][0];
        break;
      default:
        throw error("CGNSFile::discretefieldwrite: Unable to deal with specified grid location");
      }
      break;
    default:
      throw error("CGNSFile::discretefieldwrite: Unable to deal with zone type");
    }
    ierr = cgns::cg_goto(fileidx, current_zone->base->index,
                         "Zone_t", current_zone->index,
                         "DiscreteData_t", d, "end");
    errormsg(ierr, "cannot go to discrete data node");
    arraywrite(fname, cgns::Integer, ndim, dimlen, data);
  } catch (const error& e) { throw e; }
}


// -------------------------------------------------------------
// -------------------------------------------------------------
// Manipulation of Flow Solution nodes
// -------------------------------------------------------------
// -------------------------------------------------------------


// -------------------------------------------------------------
// CGNSFile::nsols
// -------------------------------------------------------------
int 
CGNSFile::nsols(void) const
{
  int ierr = 0;
  int ns;
  try { 
    ierr = cgns::cg_nsols(fileidx, current_zone->base->index,
                          current_zone->index, &ns);
    errormsg(ierr, "cannot read number of solutions");
  } catch (const error& e) { throw e; }
  return ns;
}

// -------------------------------------------------------------
// CGNSFile::solindex
// -------------------------------------------------------------
int
CGNSFile::solindex(const char *sname) const
{
  try {
    int n(nsols());
    for (int s = 1; s <= n; s++) {
      std::string nm(solinfo(s));
      if (nm == sname) return s;
    }
    std::string msg;
    msg = "cannot find solution \"";
    msg += sname;
    msg += "\"";
    errormsg(1, msg);
  } catch (const error& e) { throw e; }
  return 0;
}

// -------------------------------------------------------------
// CGNSFile::solinfo
// -------------------------------------------------------------
std::string 
CGNSFile::solinfo(const int& s) const
{
  int ierr;
  char buf[1024];
  std::string sname;
  cgns::GridLocation_t loc;
  
  try {
    ierr = cgns::cg_sol_info(fileidx, current_zone->base->index,
                             current_zone->index, s, buf, &loc);
    errormsg(ierr, "cannot read solution name");
    // errormsg(loc != cgns::CellCenter, "only CellCenter flow solution data understood");
  } catch (const error& e) { throw e; }
  sname = buf;
  return sname;
}

// -------------------------------------------------------------
// CGNSFile::solloc
// -------------------------------------------------------------
cgns::GridLocation_t 
CGNSFile::solloc(const int& s) const
{
  int ierr;
  char buf[1024];
  cgns::GridLocation_t loc;
  
  try {
    ierr = cgns::cg_sol_info(fileidx, current_zone->base->index,
                             current_zone->index, s, buf, &loc);
    errormsg(ierr, "cannot read solution name");
    // errormsg(loc != cgns::CellCenter, "only CellCenter flow solution data understood");
  } catch (const error& e) { throw e; }
  return loc;
}

// -------------------------------------------------------------
// CGNSFile::nfields
// -------------------------------------------------------------
int 
CGNSFile::nfields(const int& s) const
{
  int ierr = 0;
  int nf;

  try {
    ierr = cgns::cg_nfields(fileidx, current_zone->base->index,
                            current_zone->index, s, &nf);
    errormsg(ierr, "cannot read number of fields");
  } catch (const error& e) { throw e; }
  return nf;
}


// -------------------------------------------------------------
// CGNSFile::fieldinfo
// -------------------------------------------------------------
std::string 
CGNSFile::fieldinfo(const int& sidx, const int& fidx) const
{
  int ierr;
  cgns::DataType_t dt;
  char buf[1024];
  std::string fname;

  try {
    ierr = cgns::cg_field_info(fileidx, current_zone->base->index,
                               current_zone->index, sidx, fidx, &dt, buf);
    errormsg(ierr, "cannot read field info");
  } catch (const error& e) { throw e; }
  fname = buf;
  return fname;
}


// -------------------------------------------------------------
// CGNSFile::fieldbyname
// -------------------------------------------------------------
int 
CGNSFile::fieldbyname(const int& s, const char *name) const
{
  int fidx;
  int nf;
  try {
    nf = nfields(s);
    for (fidx = 1; fidx <= nf; fidx++) {
      std::string fname = fieldinfo(s, fidx);
      if (fname == name) break;
    }
    if (fidx > nf) {
      std::string msg;
      msg = "\"";
      msg += name;
      msg += "\": field name not found";
      errormsg(1, msg);
    }
  } catch (const error& e) { throw e; }
  return fidx;
}

// -------------------------------------------------------------
// CGNSFile::flimits
// -------------------------------------------------------------
void
CGNSFile::flimits(const int& s, cgns::cgsize_t imin[], cgns::cgsize_t imax[])
{
  int ndat;
  cgns::GridLocation_t loc;
  int idx;

  try {
    loc = solloc(s);
    switch (loc) {
    case (cgns::CellCenter):
      ndat  = current_zone->cells();
      idx = 1;
      break;
    case (cgns::Vertex):
      ndat = current_zone->vertices();
      idx = 0;
      break;
    default:
      throw CGNSFile::error("CGNSFile::flimits: unsupported solution location");
    }

    imin[0] = 1; imin[1] = 1; imin[2] = 1;
  
    if (current_zone->type == cgns::Unstructured) {
      imax[0] = ndat; imax[1] = ndat; imax[2] = ndat;
    } else {
      imax[0] = std::max<cgns::cgsize_t>(current_zone->size[idx][0], 1); 
      imax[1] = std::max<cgns::cgsize_t>(current_zone->size[idx][1], 1); 
      imax[2] = std::max<cgns::cgsize_t>(current_zone->size[idx][2], 1);
    }
  } catch (const error& e) { throw e; }
}

// -------------------------------------------------------------
// CGNSFile::fieldread
// -------------------------------------------------------------
void
CGNSFile::fieldread(const int& s, const char *fname, double *data)
{
  int ierr;
  char buf[1024];
  strncpy(buf, fname, 1024);
  try {
    cgns::cgsize_t imin[3];
    cgns::cgsize_t imax [3];
    flimits(s, imin, imax);
    ierr = cgns::cg_field_read(fileidx, current_zone->base->index,
                               current_zone->index, s, buf, 
                               cgns::RealDouble, &imin[0], &imax[0], data);
    errormsg(ierr, "cannot read field data");
  } catch (const error& e) { throw e; }
}


// -------------------------------------------------------------
// CGNSFile::solwrite
// -------------------------------------------------------------
int 
CGNSFile::solwrite(const char *sname, const cgns::GridLocation_t& sloc)
{
  int ierr = 0;
  int isol;

  try {
    ierr = cgns::cg_sol_write(fileidx, current_zone->base->index, 
                              current_zone->index, sname, sloc, &isol);
    errormsg(ierr, "cannot write flow solution");

    ierr = cgns::cg_goto(fileidx, current_zone->base->index,
                         "Zone_t", current_zone->index,
                         "FlowSolution_t", isol, "end");
    errormsg(ierr, "cannot goto to solution node just written");

    ierr = cgns::cg_dataclass_write(cgns::DataClassNull);
    errormsg(ierr, "cannot write data class");

  } catch (const error& e) { throw e; }
  return isol;
}

// -------------------------------------------------------------
// CGNSFile::fieldwrite
// -------------------------------------------------------------
int 
CGNSFile::fieldwrite(const int& sol, const char* fname, const double* data)
{
  int ierr = 0;
  int ifld; 

  try {
    ierr = cgns::cg_field_write(fileidx, current_zone->base->index,
                                current_zone->index, sol, cgns::RealDouble,
                                fname, data, &ifld);
    errormsg(ierr, "cannot write flow solution field");
  } catch (const error& e) { throw e; }
  return ifld;
}


// -------------------------------------------------------------
// CGNSFile::narbitrarymotions
// -------------------------------------------------------------
int
CGNSFile::narbitrarymotions(void) const
{
  int ierr;
  int na;

  try {
    ierr = cgns::cg_n_arbitrary_motions(fileidx, current_zone->base->index, 
                                        current_zone->index, &na);
    errormsg(ierr, "cannot read number of arbitrary motions");
  } catch (const error& e) { throw e; }
  return na;
}

// -------------------------------------------------------------
// CGNSFile::arbitrarymotionread
// -------------------------------------------------------------
void 
CGNSFile::arbitrarymotionread(const int& i, std::string& name, 
                              cgns::ArbitraryGridMotionType_t& atype)
{
  int ierr;
  char buf[1024];
  
  try {
    ierr = cgns::cg_arbitrary_motion_read(fileidx, current_zone->base->index, 
                                          current_zone->index, i, buf, &atype);
    errormsg(ierr, "cannot read arbitrary motion");
  } catch (const error& e) { throw e; }
  name = buf;
}

// -------------------------------------------------------------
// CGNSFile::arbitrarymotionwrite
// -------------------------------------------------------------
int 
CGNSFile::arbitrarymotionwrite(const char* name, const cgns::ArbitraryGridMotionType_t& atype)
{
  int ierr;
  int id;
  try {
    ierr = cgns::cg_arbitrary_motion_write(fileidx, current_zone->base->index, 
                                           current_zone->index, name, atype, &id);
    errormsg(ierr, "cannot write arbitrary motion");
  } catch (const error& e) { throw e; }
  return id;
}


// -------------------------------------------------------------
// CGNSFile::ziterread
// -------------------------------------------------------------
std::string
CGNSFile::ziterread(void) const
{
  int ierr;
  char buf[1024];
  std::string name;

  try {
    ierr = cgns::cg_ziter_read(fileidx, current_zone->base->index, 
                               current_zone->index, buf);
    errormsg(ierr, "cannot zone iterative data");
  } catch (const error& e) { throw e; }
  name = buf;
  return name;
}


// -------------------------------------------------------------
// CGNSFile::ziterwrite
// -------------------------------------------------------------
void
CGNSFile::ziterwrite(const char *name,
                     const int& n,
                     const char *sols, 
                     const char *grds, 
                     const char *agms)
{
  int ierr;
  try {
    ierr = cgns::cg_ziter_write(fileidx, current_zone->base->index, 
                                current_zone->index, name);
    errormsg(ierr, "cannot write zone iterative data");
    if (n != 0 && (sols != NULL || grds != NULL)) {
      ierr = cgns::cg_goto(fileidx, current_zone->base->index,
                           "Zone_t", current_zone->index,
                           "ZoneIterativeData_t", 1, "end");
      errormsg(ierr, "cannot goto to zone iterative zone node");
      cgns::cgsize_t idx[2];
      idx[0] = 32;
      idx[1] = n;

      if (sols != NULL) {
        ierr = cgns::cg_array_write("ZoneSolutionPointers", 
                                    cgns::Character, 2, idx, sols);
        errormsg(ierr, "cannot solution pointer in zone iterative data");
      }

      if (grds != NULL) {
        ierr = cgns::cg_array_write("GridCoordinatesPointers", 
                                    cgns::Character, 2, idx, grds);
        errormsg(ierr, "cannot grid pointer in zone iterative data");
      }

      if (agms != NULL) {
        ierr = cgns::cg_array_write("ArbitraryGridMotionPointers ", 
                                    cgns::Character, 2, idx, agms);
        errormsg(ierr, "cannot abitrary grid motion pointer in zone iterative data");
      }
    }
  } catch (const error& e) { throw e; }
}

// -------------------------------------------------------------
// CGNSFile::bitersize
// -------------------------------------------------------------
/** 
 * This routine reads the BaseIterativeData node from the current base
 * and determines the sizes of information in the node. This provides
 * the caller with an indication of what data is available in the
 * BaseIterativeData node and enough information to size arrays for a
 * subsequent call to biterread().  
 * 
 * @param nsteps number of time steps (or 0 if no node found)
 * @param ntimes number of times (equal to \c nsteps if TimeValues array found, otherwize 0)
 * @param maxzptr maximum number of zones in a single step (0 if no ZonePointers array found)
 */
void
CGNSFile::bitersize(int& nsteps, int& ntimes, int& maxzptr)
{
  int ierr;
  std::string name;

  try {
    biterread(name, nsteps);
  } catch (const error& e) {
    switch (e.code()) {
    case CG_NODE_NOT_FOUND:
      nsteps = 0;
      ntimes = 0;
      maxzptr = 0;
      return;
      break;
    default:
      throw e;
    }
  }

  nsteps = 0;
  ntimes = 0;
  maxzptr = 0;
  try {
    ierr = cgns::cg_goto(fileidx, current_base->index,
                         "BaseIterativeData_t", 1, "end");
    errormsg(ierr, "biterread: cannot goto base iterative data node");

    int na = narrays();
    for (int ia = 1; ia < na; ia++) {
      ArrayPtr array(arrayinfo(ia));
      if (array->name == "TimeValues") {
        ntimes = array->dimvector[0];
      } else if (array->name == "NumberOfZones") {
        std::vector<int> nzone(array->dimvector[0]);
        arrayread(ia, &nzone[0], cgns::Integer);
        for (int i = 0; i < array->dimvector[0]; i++) {
          if (nzone[i] > maxzptr) { maxzptr = nzone[i]; }
        }
      }
    }
  } catch (const error& e) { throw e; }
}

// -------------------------------------------------------------
// CGNSFile::biterread
// -------------------------------------------------------------
/** 
 * bitersize() should be used to determine if the node exists and the
 * array sizes necessary to hold the node's information.
 *
 * If any of these arrays are requested but do not exist, an exception
 * is thrown.  Pointers must point to a correctly sized chunk of memory.
 * 
 * @param name name of the BaseIterativeData zone
 * @param nsteps number of steps 
 * @param times array to store the \c TimeValues array
 * @param numzone array to store the \c NumberOfZones array
 * @param zoneptr array to store the \c ZonePointers array
 */
void
CGNSFile::biterread(std::string& name, int& nsteps, double *times, 
                    int* numzone, char* zoneptr)
{
  int ierr;
  char buf[1024];
  int i;

  try {
    ierr = cgns::cg_biter_read(fileidx, current_base->index, 
                               &buf[0], &i);
    errormsg(ierr, "biterread: cannot read base iterative data node");

    name = buf;
    nsteps = i;

    ierr = cgns::cg_goto(fileidx, current_base->index,
                         "BaseIterativeData_t", 1, "end");
    errormsg(ierr, "biterwrite: cannot goto base iterative data node");

    int na = narrays();

    for (int ia = 1; ia < na; ia++) {
      ArrayPtr array(arrayinfo(ia));

      if (array->name == "TimeValues" && times != NULL) {
        arrayread(ia, &times[0], cgns::RealDouble);
      } else if (array->name == "NumberOfZones" && numzone != NULL) {
        arrayread(ia, &numzone[0], cgns::Integer);
      } else if (array->name == "ZonePointers" && zoneptr != NULL) {
        arrayread(ia, &zoneptr[0], cgns::Character);
      }
    }
  } catch (const error& e) { throw e; }
}

// -------------------------------------------------------------
// CGNSFile::biterwrite
// -------------------------------------------------------------
/** 
 * This routine writes a BaseIterativeData node to the current base
 * node.  At a minimum, the number of time steps in the solution is
 * required.  Additionally, the actual simulation times, and changing
 * zone information may be specified.
 * 
 * @param nsteps number of solution steps (required)
 * @param name name of the node (optional)
 * @param times array (\c nsteps long) containing time for each simulation step (optional)
 * @param numzone array (\c nsteps long) containing the number of zones used in  each simulation step (optional)
 * @param zoneptr array containing the zone names for each each simulation step (optional)
 */
void
CGNSFile::biterwrite(const cgns::cgsize_t& nsteps, const char* name, const double *times,
                     const int* numzone, const char* zoneptr)
{
  int ierr;
  const char *defname = "BaseIterativeData";
  if (name != NULL) defname = name;
  
  try {
    ierr = cgns::cg_biter_write(fileidx, current_base->index, name, nsteps);
    errormsg(ierr, "biterwrite: cannot write base iterative data node");

    if (times != NULL || numzone != NULL || zoneptr != NULL) {
      ierr = cgns::cg_goto(fileidx, current_base->index,
                           "BaseIterativeData_t", 1, "end");
      errormsg(ierr, "biterwrite: cannot goto base iterative data node");
    }

    cgns::cgsize_t idx = nsteps;

    if (times != NULL) {
      ierr = cgns::cg_array_write("TimeValues", cgns::RealDouble, 1, &idx, times);
      errormsg(ierr, "biterwrite: cannot write solution times");
    }

    if (numzone != NULL) {
      ierr = cgns::cg_array_write("NumberOfZones", cgns::Integer, 1, &idx, numzone);
      errormsg(ierr, "biterwrite: cannot write zone numbers");

      int i;
      for (i = 0, idx = 0; i < nsteps; i++) {
        if (numzone[i] > idx) idx = numzone[i];
      }

      if (zoneptr != NULL) {
        cgns::cgsize_t zidx[3] = { 32, idx, nsteps };
        ierr = cgns::cg_array_write("ZonePointers", cgns::Character, 3, &zidx[0], zoneptr);
        errormsg(ierr, "biterwrite: cannot write zone numbers");
      }
    }

      
  } catch (const error& e) { throw e; }
}


// -------------------------------------------------------------
// CGNSFile::none2one
// -------------------------------------------------------------
int
CGNSFile::none2one(void) const
{
  int ierr;
  int result;

  try {
    ierr = cgns::cg_n1to1(fileidx, current_zone->base->index, 
                           current_zone->index, &result);
    errormsg(ierr, "cannot read number of 1-to-1 connections");
    
  } catch (const error& e) { throw e; }

  return (result);
}  

// -------------------------------------------------------------
// CGNSFile::nconns
// -------------------------------------------------------------
int 
CGNSFile::nconns(void) const
{
  int ierr;
  int result;

  try {
    ierr = cgns::cg_nconns(fileidx, current_zone->base->index, 
                           current_zone->index, &result);
    errormsg(ierr, "cannot read number of connections");
    
  } catch (const error& e) { throw e; }

  return (result);
}


// -------------------------------------------------------------
// CGNSFile::newconn
// -------------------------------------------------------------
CGNSFile::ConnPtr
CGNSFile::newconn(void) const
{
  ConnPtr c(new Conn(current_zone));
  return c;
}

// -------------------------------------------------------------
// CGNSFile::conninfo
// -------------------------------------------------------------
CGNSFile::ConnPtr
CGNSFile::conninfo(const int& c) const
{
  int ierr;
  char cnm[1024], dnm[1024];
  ConnPtr conn(newconn());
  try {
    ierr = cgns::cg_conn_info(fileidx, current_zone->base->index,
                              current_zone->index, c, 
                              cnm, 
                              &(conn->location),
                              &(conn->conntype), 
                              &(conn->ptsettype),
                              &(conn->npnts), 
                              dnm,
                              &(conn->donor_zonetype),
                              &(conn->donor_ptset_type),
                              &(conn->donor_datatype),
                              &(conn->ndata_donor));

    if (!my_disable_conn_error) {
      errormsg(ierr, "unable to read connection info");
    }
    conn->name = cnm;
    conn->donorname = dnm;

    // See if the connection is periodic

    conn->periodic = false;

    float rcenter[3], rangle[3], translat[3];
    ierr = cgns::cg_conn_periodic_read(fileidx,
                                       conn->zone->base->index,
                                       conn->zone->index,
                                       c, rcenter, rangle, translat);
    if (ierr == CG_OK) {
      conn->periodic = true;
      for (int i = 0; i < conn->zone->base->physdim; i++) {
        conn->periodic_rcenter[i] = rcenter[i];
        conn->periodic_rotation[i] = rangle[i];
        conn->periodic_tranlation[i] = translat[i];
      }
    }

    ierr = cgns::cg_goto(fileidx, 
                         conn->zone->base->index, 
                         "Zone_t",  conn->zone->index,
                         "ZoneGridConnectivity_t", 1,
                         "GridConnectivity_t", c, 
                         "end");
    errormsg(ierr, "cannot go to grid connection node");

    int ndesc = ndescriptors();
    
    if (ndesc > 0) {
      for (int nd = 1; nd <= ndesc; nd++) {
        char namebuf[1024];
        char valbuf[1024];
        descriptorread(nd, namebuf, valbuf, 1024);
        if (conndescname == namebuf) {
          conn->donorconnname = valbuf;
          break;
        }
      }
    }
  } catch (const error& e) { throw e; }
  return conn;
}


// -------------------------------------------------------------
// CGNSFile::connread
// -------------------------------------------------------------
/** 
 * 
 * 
 * @param c connection index in current zone
 * @param target place to put target point information (this zore)
 * @param donor place to put donor point/cell information (other zone)
 * @param interp place to put the InterpolantsDonor array (if any)
 */
bool
CGNSFile::connread(const int& c, cgns::cgsize_t *target, cgns::cgsize_t *donor, double *interp) const
{
  int ierr;
  bool hasinterp = false;
  try {
    const ConnPtr conn(conninfo(c));
    int bidx = conn->zone->base->index;
    int zidx = conn->zone->index;
    ierr = cgns::cg_conn_read(fileidx, bidx, zidx, c,
                              &target[0], conn->donor_datatype, &donor[0]);
    errormsg(ierr, "cannot read grid connection");
    
    ierr = cgns::cg_goto(fileidx, 
                         bidx, 
                         "Zone_t",  zidx,
                         "ZoneGridConnectivity_t", 1,
                         "GridConnectivity_t", c, 
                         "end");
    errormsg(ierr, "cannot go to grid connection node");

    if (conn->donor_ptset_type == cgns::CellListDonor &&
        interp != NULL) {
      int na = narrays();
      if (na > 0) {
        for (int ni = 1; ni <= na; ni++) {
          ArrayPtr a(arrayinfo(ni));
          if (a->name == "InterpolantsDonor") {
            arrayread(ni, interp, cgns::RealDouble);
            hasinterp = true;
            break;
          }
        }
      }
    } 

  } catch (const error& e) { throw e; }
  return (hasinterp);
}

// -------------------------------------------------------------
// CGNSFile::connperiodicwrite
// -------------------------------------------------------------
void 
CGNSFile::connperiodicwrite(const int& c, const double* inrc, const double* inra, const double* intr)
{
  int ierr;
  float rc[3], ra[3], tr[3];
  for (int i = 0; i < 3; i++) {
    rc[i] = inrc[i];
    ra[i] = inra[i];
    tr[i] = intr[i];
  }
  ierr = cgns::cg_conn_periodic_write(fileidx,
                                      current_zone->base->index,
                                      current_zone->index,
                                      c, rc, ra, tr);
  errormsg(ierr, "unable to write periodic connection properties");
}



// -------------------------------------------------------------
// CGNSFile::connwrite
// -------------------------------------------------------------
int
CGNSFile::connwrite(const ConnPtr& conn, cgns::cgsize_t *tpts, cgns::cgsize_t *dpts, double *interp)
{
  int ierr;
  int cidx;
  try {
    if (conn->ndata_donor > 0) {
      ierr = cgns::cg_conn_write(fileidx, conn->zone->base->index, 
                                 conn->zone->index, 
                                 conn->name.c_str(),
                                 conn->location,
                                 conn->conntype,
                                 conn->ptsettype,
                                 conn->npnts,
                                 tpts,
                                 conn->donorname.c_str(),
                                 conn->donor_zonetype,
                                 conn->donor_ptset_type,
                                 conn->donor_datatype,
                                 conn->ndata_donor,
                                 dpts,
                                 &cidx);
    } else {
      ierr = cgns::cg_conn_write_short(fileidx, 
                                       conn->zone->base->index, 
                                       conn->zone->index, 
                                       conn->name.c_str(),
                                       conn->location,
                                       conn->conntype,
                                       conn->ptsettype,
                                       conn->npnts,
                                       tpts,
                                       conn->donorname.c_str(),
                                       &cidx);
    }
                                       
    errormsg(ierr, "unable to write grid connection");

    if (conn->periodic) {
      this->connperiodicwrite(cidx, 
                              conn->periodic_rcenter, 
                              conn->periodic_rotation, 
                              conn->periodic_tranlation);
    }

    ierr = cgns::cg_goto(fileidx, 
                         conn->zone->base->index, 
                         "Zone_t",  conn->zone->index,
                         "ZoneGridConnectivity_t", 1,
                         "GridConnectivity_t", cidx, 
                         "end");
    errormsg(ierr, "cannot go to grid connection node");

    if (conn->donor_ptset_type == cgns::CellListDonor &&
        interp != NULL && 
        false) {

      int dimlen;
      cgns::cgsize_t dims[2];
      switch (conn->donor_zonetype) {
      case (cgns::Structured):
        dimlen = 2;
        dims[0] = conn->zone->base->celldim;
        dims[1] = conn->ndata_donor;
        break;
      case (cgns::Unstructured):
        dimlen = 1;
        dims[0] = conn->ndata_donor;
        break;
      default:
        break;
      }
      arraywrite("InterpolantsDonor", cgns::RealDouble, dimlen, &dims[0], (void *)interp);
    }

    if (conn->donorconnname.length() > 0) {
      descriptorwrite(conndescname.c_str(), conn->donorconnname.c_str());
    }

  } catch (const error& e) { throw e; }
  return cidx;
}


// -------------------------------------------------------------
// CGNSFile::ndescriptors
// -------------------------------------------------------------
int
CGNSFile::ndescriptors(void) const
{
  int ierr;
  int ndesc;
  try {
    ierr = cgns::cg_ndescriptors(&ndesc);
    errormsg(ierr, "cannot get number of descriptors");
  } catch (const error& e) { throw e; }
  return ndesc;
}

// -------------------------------------------------------------
// CGNSFile::descriptorread
// -------------------------------------------------------------
void
CGNSFile::descriptorread(const int& idx, char* name, char* text, const int& textlen) const
{
  int ierr;
  char *val;

  try {
    ierr = cgns::cg_descriptor_read(idx, name, &val);
    errormsg(ierr, "error reading descriptor");
  } catch (const error& e) { throw e; }
  strncpy(text, val, textlen);
  free(val);
}

// -------------------------------------------------------------
// CGNSFile::descriptorwrite
// -------------------------------------------------------------
void
CGNSFile::descriptorwrite(const char* name, const char* text)
{
  int ierr;

  try {
    ierr = cgns::cg_descriptor_write(name, text);
    errormsg(ierr, "cannot write descriptor");
  } catch (const error& e) { throw e; }
}
  
// -------------------------------------------------------------
// CGNSFile::islink
// -------------------------------------------------------------
int
CGNSFile::islink(void)
{
  int ierr;
  int plen(0);
  try {
    ierr = cgns::cg_is_link(&plen);
    errormsg(ierr, "cannot determine if node is link");
  } catch (const error& e) { throw e; }
  return plen;
}

// -------------------------------------------------------------
// CGNSFile::readlink
// -------------------------------------------------------------
void 
CGNSFile::linkread(std::string& fname, std::string& pname)
{
  int ierr;
  try {
    char *fnamebuf, *pnamebuf;
    ierr = cgns::cg_link_read(&fnamebuf, &pnamebuf);
     errormsg(ierr, "cannot write link");
    fname = fnamebuf;
    pname = pnamebuf;
    ierr = cgns::cg_free(fnamebuf);
    errormsg(ierr, "free error writing link (1)");
    ierr = cgns::cg_free(pnamebuf);
    errormsg(ierr, "free error writing link (2)");
  } catch (const error& e) { throw e; }
}

// -------------------------------------------------------------
// CGNSFile::writelink
// -------------------------------------------------------------
void 
CGNSFile::linkwrite(const char* nname, const char* fname, const char* pname)
{
  int ierr;
  try {
    ierr = cgns::cg_link_write(nname, fname, pname);
    errormsg(ierr, "cannot write link");
  } catch (const error& e) { throw e; }
}


// -------------------------------------------------------------
// CGNSFile::iscgns
// -------------------------------------------------------------
bool
CGNSFile::iscgns(const std::string& name)
{
  int ierr;

#if CGNS_VERSION > 3000
  int cgnstype;
  ierr = cgns::cg_is_cgns(name.c_str(), &cgnstype);
#else
  ierr = cgns::cg_is_cgns(name.c_str());
#endif
  return ierr == CG_OK;
}
