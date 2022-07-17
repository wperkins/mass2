// Emacs Mode Line: -*- Mode:c++;-*-
// -------------------------------------------------------------
/**
 * @file   cgnsfile.h
 * @author William A. Perkins
 * @date Fri Jun 29 07:55:51 2012
 * 
 * @brief  Declaration of class CGNSFile
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
// Last Change: 2018-04-24 11:13:18 d3g096
// -------------------------------------------------------------

// SCCS ID: $Id$ Battelle PNL

#ifndef _cgnsfile_h_
#define _cgnsfile_h_

#include <string>
#include <stdexcept>
#include <map>
#include <boost/shared_ptr.hpp>

namespace cgns {
#include <cgnslib.h>
#if CGNS_VERSION < 3100
  typedef int cgsize_t;
#endif
}

// -------------------------------------------------------------
//  class cgnsfile
// -------------------------------------------------------------
/// Generic CGNS file.
/**
 * This class is an encapsulation of a CFD General Notation System
 * (CGNS) format file.  The interface mostly emulates that of the
 * Python interface to the CGNS libary (PyCGNS).
 * 
 */
class CGNSFile {
public:

  // -------------------------------------------------------------
  //  class cgnsfile::error
  // -------------------------------------------------------------
  /// Exception to indicate a CGNS file error.
  /**
   * This exception is thrown anytime something goes wrong with writing
   * or reading a CGNS file.
   * 
   */
  class error : public std::runtime_error {
  protected:
    int mycode;                 /**< The CGNS error code when thrown */
  public:

    /// The default constructor.
    /** 
     * This is basically 
     * 
     * @param msg the error message
     * 
     * @return a new error instance
     */  
    error(const std::string& msg, const int& code = -1) 
      : std::runtime_error(msg), mycode(code) 
    { };
    
    /// The copy constructor.
    error(const error& old) 
      : std::runtime_error(old), mycode(old.mycode) 
     {};
    
    /// The destructor
    ~error(void) throw() {};

    /// Get the error code
    const int& code(void) const { return mycode; };
  };  

  // -------------------------------------------------------------
  // struct Base
  // -------------------------------------------------------------
  /// A container for base node information.
  /**
   * This holds the information contained in the base node of the CGNS
   * file.
   * 
   */
  struct Base {
    int index;
    std::string name;
    int celldim;
    int physdim;
    Base() : index(0), name(), celldim(0), physdim(0) {};
    Base(const Base& old) 
      : index(old.index), name(old.name), 
        celldim(old.celldim), physdim(old.physdim) {};
    ~Base() {};
  };

  /// Smart pointer for struct Base.
  typedef boost::shared_ptr<Base> BasePtr;


  // -------------------------------------------------------------
  // struct Zone
  // -------------------------------------------------------------
  /// A container for zone information.
  /**
   * This holds zone specific information.
   * 
   */
  struct Zone {
    const BasePtr base; /**< Base node containing this zone */
    int index;
    std::string name;
    cgns::cgsize_t size[3][3];
    cgns::ZoneType_t type;
    Zone(const CGNSFile::BasePtr b) : base (b), index(0), name() {
      for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
          size[i][j] = 0;
        }
      }
    };
    ~Zone() {};
    int vertices(void) const;
    int cells(void) const;
    void setsize(const cgns::cgsize_t *size);
  };

  /// Smart pointer for struct Zone.
  typedef boost::shared_ptr<Zone> ZonePtr;


  // -------------------------------------------------------------
  // Struct Section
  // -------------------------------------------------------------
  /// A container for element connectivity "sections"
  struct Section {
    const ZonePtr zone;        /**< Zone containing this zone. */
    int index;                  /**< Index of this section */
    std::string name;                /**< Name of section */
    cgns::ElementType_t type;   /**< Type of elements in this section */
    cgns::cgsize_t idxbeg;      /**< Starting element index */
    cgns::cgsize_t idxend;      /**< Ending element index */
    int nbnd;                   /**< Number of boundaries */
    bool parent;                /**<  */

    Section(const ZonePtr& z) : zone(z), index(0), name(), type(cgns::MIXED), 
                                idxbeg(0), idxend(0), nbnd(0), parent(false) { };
    ~Section(void) {};

    cgns::cgsize_t size(void) { return idxend - idxbeg + 1; } ;
    cgns::cgsize_t cell_size(void) { return size() - nbnd; } ;
  };

  /// Smart pointer to a Section
  typedef boost::shared_ptr<Section> SectionPtr;

  // -------------------------------------------------------------
  // Struct Array
  // -------------------------------------------------------------
  /// A container for data array information.
  /**
   * This holds enough information about an existing data array to
   * correctly size an array of the appropriate type.
   * 
   */
  struct Array {
    int index;
    std::string name;
    cgns::DataType_t datatype;
    int dim;
    cgns::cgsize_t dimvector[3];
    Array(void) 
      : index(0), name(), datatype(cgns::RealDouble), dim(3)
    { dimvector[0] = 0; dimvector[1] = 0; dimvector[2] = 0; };
    ~Array(void) {};
    cgns::cgsize_t size(void) const;
  };

  /// Smart pointer for struct Array
  typedef boost::shared_ptr<Array> ArrayPtr;

  // -------------------------------------------------------------
  //  class BC
  // -------------------------------------------------------------
  struct BC {
    const ZonePtr zone;        /**< The zone to which this boundary condition applies. */
    std::string name;           /**< The name of this boundary condition. */
    int index;                  /**< The index of this boundary condition. */
    cgns::BCType_t type;        /**< The type of this boundary condition. */
    cgns::PointSetType_t pttype; /**<  */
    cgns::GridLocation_t location; /**<  */
    cgns::cgsize_t npts;
    int ndatasets;
    int normidx[3];
    cgns::cgsize_t normidxflag;
    cgns::DataType_t normtype;
    BC(const ZonePtr &z) 
      : zone(z), name(), index(0), type(cgns::BCWall), pttype(cgns::PointList), 
        location(cgns::Vertex), npts(0), ndatasets(0), normidxflag(0)
    {};
    ~BC(void) {};

  };
  
  typedef boost::shared_ptr<BC> BCPtr;
    
  // -------------------------------------------------------------
  // class Conn
  // -------------------------------------------------------------
  struct Conn {
    const ZonePtr zone;         /**< The zone containing this connection */
    std::string name;           /**< The name of this connection */
    cgns::GridLocation_t location; /**< The location type of this connection (usually cgns::Vertex) */
    cgns::GridConnectivityType_t conntype; /**< The type of connection */
    cgns::PointSetType_t ptsettype; /**< How the list of points is specified  */
    cgns::cgsize_t npnts;       /**< The number of points involved */
    std::string donorname;      /**< The name of the connecting zone */
    std::string donorconnname;  /**< The name of the connection in the connection zone */
    cgns::ZoneType_t donor_zonetype; /**< The type of zone connecting to */
    cgns::PointSetType_t donor_ptset_type; /**< How the donor points are specified */
    cgns::DataType_t donor_datatype; /**< Always cgns::Integer */
    cgns::cgsize_t ndata_donor;            /**< The number of donor points (always the same as npnts) */
    bool periodic;
    double periodic_rcenter[3], periodic_rotation[3], periodic_tranlation[3];
    Conn(const ZonePtr& z) 
      : zone(z), name(), location(cgns::Vertex), conntype(cgns::Abutting),
        ptsettype(cgns::PointList), npnts(0), donorname(), donorconnname(),
        donor_zonetype(cgns::ZoneTypeNull),
        donor_ptset_type(cgns::PointSetTypeNull), donor_datatype(cgns::Integer), 
        ndata_donor(0), periodic(false)
    {
      for (int i = 0; i < 3; i++) {
        periodic_rcenter[i] = 0.0;
        periodic_rotation[i] = 0.0;
        periodic_tranlation[i] = 0.0;
      }
    };
    ~Conn(void) {};
  };

  /// A shared pointer to Conn instances
  typedef boost::shared_ptr<Conn> ConnPtr;

  /// A routine to see if an existing file in a readable CGNS file
  static bool iscgns(const std::string& name);

protected:
  int fileidx;                  /**< The CGNS file index. */

  std::string myfilename;       /**< The file name. */

  int mymode;                   /**< How was this file opened */

  BasePtr current_base;         /**< Information for the current base node. */

  ZonePtr current_zone;         /**< Information for the current zone. */

  /// A place to keep track of the indexes of written bases
  std::map<int, BasePtr> base_index_cache;

  /// A place to keep track of the names of written bases
  std::map<std::string, BasePtr> base_name_cache;

  /// A place to keep track of the indexes of written zones
  /**
   * When the file is open CG_MODE_WRITE, you can only refer to a zone by
   * index.  This is used to cache zone info when it is written so it
   * can be used by the ::zone methods instead of ::zoneread
   * 
   */
  std::map<int, ZonePtr> zone_index_cache;


  /// A place to keep track of the names of written zones
  /**
   * When the file is open CG_MODE_WRITE, you can only refer to a zone by
   * index.  This is used to cache zone info when it is written so it
   * can be used by the ::zone methods instead of ::zoneread
   * 
   */
  std::map<std::string, ZonePtr> zone_name_cache;

  /// Disable catching of connection read errors (from @c cg_conn_info)
  bool my_disable_conn_error;

  // -------------------------------------------------------------
  // Manipulation of data arrays (DataArray_t nodes)
  // -------------------------------------------------------------

  /// Get the number of data arrays in the current node.
  int narrays(void) const;

  /// Get information for a specific data array in current node.
  ArrayPtr arrayinfo(const int& i) const;

  /// Read a data array.
  void arrayread(const int& i, void *data, 
                 cgns::DataType_t astype = cgns::RealDouble) const;

  /// Write a data array.
  void arraywrite(const char* name, const cgns::DataType_t& type, 
                  const int& dims, const cgns::cgsize_t *dimlen, const void *data);

  /// Establish field array limits
  void flimits(const int& s, cgns::cgsize_t imin[], cgns::cgsize_t imax[]);

  CGNSFile(const CGNSFile& old);

public:
  CGNSFile(void);
  CGNSFile(const std::string& name, const int& m = CG_MODE_READ);
  virtual ~CGNSFile(void);

  /// A generic method used to throw error exceptions.
  void errormsg(const int& ierr, const std::string& msg) const throw(error);

  /// Get the name of the CGNS file.
  const std::string& name(void) const { return myfilename; };

  /// Get the CGNS file id (to call CGNS library directly)
  int file_index(void) const { return fileidx; };

  /// Open the specified file and prepare it for writing.
  virtual void open(const std::string& name, const int& m = CG_MODE_READ);

  /// Close the file.
  void close(void);

  /// Disable error connection error checking
  void disable_connection_error(void) { my_disable_conn_error = true; }
  void enable_connection_error(void) { my_disable_conn_error = false; }

  // -------------------------------------------------------------
  // Manipulation of Descriptor nodes
  // -------------------------------------------------------------
  
  /// Get the number of descriptors in the current node
  int ndescriptors(void) const;

  /// Create or update a descriptor in the current node
  void descriptorwrite(const char* name, const char* text);

  /// Get a descriptor in the current node
  void descriptorread(const int& idx, char* name, char* text, const int& textlen) const;

  // -------------------------------------------------------------
  // Manipulation of the base node
  // -------------------------------------------------------------

  /// Get the number of bases in the file.
  int bases(void) const;

  /// Read the info for a specific base from the file (makes base current).
  const BasePtr& baseread(const int& i);

  /// Write a base node to the file (makes base current).
  const BasePtr& basewrite(const char *name, const int& cdim, const int& pdim);

  /// Set the base node to be used (by id).
  const BasePtr& base(const int& b);

  //// Set the base node to be used (by name).
  const BasePtr& base(const std::string& name);

  // -------------------------------------------------------------
  // Manipulation of Miscellaneous Data Structures
  // -------------------------------------------------------------

  /// Read the simulation type
  cgns::SimulationType_t simulationtyperead(void);

  /// Write the simulation type
  void simulationtypewrite(const cgns::SimulationType_t& t); 

  /// Read the gravity vector from the current base node.
  void gravityread(double *g);

  /// Write a gravity vectory to the current base node.
  void gravitywrite(const double *g);

  // -------------------------------------------------------------
  // Manipulation of zones
  // -------------------------------------------------------------

  /// Get the number of zones in the current base node.
  int zones(void) const;

  /// Read information for the specified zone (makes zone current).
  const ZonePtr& zoneread(const int& i);

  /// Write a new zone (makes new zone current).
  const ZonePtr& zonewrite(const char *name, cgns::cgsize_t *size, const cgns::ZoneType_t& type);

  /// Set current zone (by id).
  const ZonePtr& zone(const int& z);

  // Set current zone (by name).
  const ZonePtr& zone(const std::string& name);

  /// Get the current zone
  const ZonePtr& zone(void) const;

  /// Get the total number of cells in the file
  int cells(void);

  /// Get the total number of vertices in the file
  int vertices(void);

  // -------------------------------------------------------------
  // Manipulation of grids (in the current zone)
  // -------------------------------------------------------------
  
  /// Get the number of grids in the current zone.
  int grids(void) const;

  /// Get the index of the named grid.
  int gridindex(const char *gridname = NULL) const;

  /// Read the specified grid in the current zone.
  std::string gridread(const int& i) const;

  /// Write a new grid in the current zone.
  int gridwrite(const char *name);

  // -------------------------------------------------------------
  // Manipulation of grid coordinates (in current zone).
  // -------------------------------------------------------------

  /// Get number of coordinates (in the current zone)
  int ncoords(const char *gridname = NULL) const;

  /// Get coordinate info (by index)
  ArrayPtr coordinfo(const int& i, const char *gridname = NULL);

  /// Read a coordinate (by name) and return in a new array
  int coordread(const char *coordname, 
                double *coord, 
                const char *gridname = NULL);

  /// Read a coordinate (by index) and return in a new array
  void coordread(const int& i,
                 double *coord, 
                 const char *gridname = NULL);

  /// Write a coordinate array (with type)
  void coordwrite(const char *coordname, 
                  const void *coord,
                  const int& gridindex = 0,
                  const cgns::DataType_t coordtype = cgns::RealDouble);

  // -------------------------------------------------------------
  // Manipulation of element connectivity
  // -------------------------------------------------------------

  /// Get vertices per element, given element type
  int npe(const cgns::ElementType_t& t);

  /// Get the number of sections in the current zone.
  int nsections(void);

  /// Read a Section from the current zone
  SectionPtr sectionread(const int& i);

  /// Create a new Section
  SectionPtr newsection(void);

  /// Write a Section
  void sectionwrite(SectionPtr& s, cgns::cgsize_t *e, cgns::cgsize_t *p = NULL);

  /// Get the size of the element connectivity 
  int elementdatasize(const SectionPtr& s);

  /// Get elements in specfied section
  void elementsread(const SectionPtr& s, cgns::cgsize_t *e, cgns::cgsize_t *p);

  // -------------------------------------------------------------
  // Manipulation of boundary conditions
  // -------------------------------------------------------------

  /// Get the number of boundary conditions for the current zone
  int nbc(void);

  /// Get boundary condition information.
  BCPtr bcinfo(const int& bc) const;

  /// Read boundary condition pointlist
  void bcread(const int& bc, cgns::cgsize_t *pts) const;

  /// Make a new boundary condition for the current zone
  BCPtr newbc(void) const;

  /// Write boundary condition
  void bcwrite(BCPtr& bp, const cgns::cgsize_t *pnts);

  // -------------------------------------------------------------
  // Manipulation of Discrete Data
  // -------------------------------------------------------------

  /// Get the number of DiscreteData nodes in the current zone
  int ndiscrete(void) const;
  
  /// Read the specified DiscreteData node (by index)
  void discreteread(const int& n, std::string& name, cgns::GridLocation_t& loc) const;

  /// Find the specified DiscreteData node (by name)
  void discreteread(const char *name, int& idx, cgns::GridLocation_t& loc) const;
  
  /// Write a DiscreteData node in the current zone
  int discretewrite(const char *name, const cgns::GridLocation_t loc = cgns::Vertex);

  /// Get the number of data fields in the DiscreteData node
  int ndiscretefields(const int& d) const;

  /// Get name of specified field in a DiscreteData node
  void discretefieldinfo(const int& d, const int& fidx, std::string& name, cgns::DataType_t& ftype) const;

  /// Get index of specified field in a DiscreteData node
  int discretefieldbyname(const int& d, const char *name) const;

  /// Read the specified field from a DiscreteData node (integer)
  void discretefieldread(const int& d, const char *fname, int *data) const;

#if CG_BUILD_64BIT
  /// Read the specified field from a DiscreteData node (cgns::cgsize_t)
  void discretefieldread(const int& d, const char *fname, cgns::cgsize_t *data) const;
#endif 

  /// Read the specified field from a DiscreteData node (double)
  void discretefieldread(const int& d, const char *fname, double *data) const;

  /// Write the specified data to a field in a DiscreteData node (integer)
  void discretefieldwrite(const int& d, const char *fname, 
                                    const cgns::GridLocation_t& loc, const int *data);

#if CG_BUILD_64BIT
  /// Write the specified data to a field in a DiscreteData node (cgns::cgsize_t)
  void discretefieldwrite(const int& d, const char *fname, 
                                    const cgns::GridLocation_t& loc, const cgns::cgsize_t *data);
#endif 

  /// Write the specified data to a field in a DiscreteData node (double)
  void discretefieldwrite(const int& d, const char *fname, 
                                    const cgns::GridLocation_t& loc, const double *data);

  // -------------------------------------------------------------
  // Manipulation of flow solutions
  // -------------------------------------------------------------
  
  /// Get number of flow solutions in current zone
  int nsols(void) const;

  /// Find a solution by name
  int solindex(const char *sname) const;

  /// Get  information for a specific solution
  std::string solinfo(const int& s) const;

  /// Get the grid location for a specific solution
  cgns::GridLocation_t solloc(const int& s) const;

  /// Get number of data fields from a specific solution in the current zone
  int nfields(const int& s) const;

  /// Get information about a specific solution field
  std::string fieldinfo(const int& sidx, const int& fidx) const;

  /// Get the index of a field given its name
  int fieldbyname(const int& s, const char *name) const;

  /// Get a flow solution by name
  void fieldread(const int& s, const char *name, double *data);

  /// Get a flow solution by its index
  // void fieldread(const int& s, const int& fidx, double *data);

  /// Write a new solution to the current zone
  int solwrite(const char *sname, 
               const cgns::GridLocation_t& sloc = cgns::CellCenter);

  /// Write a new field to the specified solution
  int fieldwrite(const int& sol, const char* fname, const double* data);

  /// Write a new field to the specified solution
  int fieldwrite(const int& sol, const char* fname, const float* data);

  // -------------------------------------------------------------
  // Manipulation of Grid Connectivity
  // -------------------------------------------------------------

  /// Get the number of 1-to-1 grid connections in the current zone
  int none2one(void) const;

  /// Get the number of grid connections in the current zone
  int nconns(void) const;

  /// Get information on the current connection
  ConnPtr conninfo(const int& c) const;

  /// Make a new Conn for the current zone
  ConnPtr newconn(void) const;

  /// Read the specified grid connection
  bool connread(const int& c, cgns::cgsize_t *target, cgns::cgsize_t *donor, double *interp = NULL) const;

  /// Write the specified grid connection
  int connwrite(const ConnPtr& conn, cgns::cgsize_t *tpts, cgns::cgsize_t *dpts, double *interp = NULL);

  /// Write special periodic properties for the specified connection
  void connperiodicwrite(const int& c, const double* rc, const double* ra, const double* tr);

  // -------------------------------------------------------------
  // Manipulation of iterative data
  // -------------------------------------------------------------

  /// Get the sizes of things in the base iterative data node
  void bitersize(int& nsteps, int& ntimes, int& maxzptr);

  /// Read the iterative data node in the current base (if any)
  void biterread(std::string& name, 
                 int& nsteps, 
                 double *times = NULL, 
                 int* numzone = NULL,
                 char* zoneptr = NULL);

  /// Write an iterative data node in the current base
  void biterwrite(const cgns::cgsize_t& nsteps, 
                  const char* name = NULL, 
                  const double *times = NULL,
                  const int* numzone = NULL,
                  const char* zoneptr = NULL);

  /// Read the zone iterative data node in the current zone (if any)
  std::string ziterread(void) const;

  /// Write a zone iterative data node in the current zone
  void ziterwrite(const char* name, 
                  const int& n = 0,
                  const char* sols = NULL,
                  const char* grds = NULL,
                  const char* agms = NULL);

  // -------------------------------------------------------------
  // Manipulation of grid movement data
  // -------------------------------------------------------------

  /// Get the number of arbitray grid motion nodes in the current zone
  int narbitrarymotions(void) const;

  /// Read the specified arbitrary grid motion node in the current zone
  void arbitrarymotionread(const int& i, std::string& name, 
                           cgns::ArbitraryGridMotionType_t& atype);

  /// Write a new arbitrary grid motion node in the current zone.
  int arbitrarymotionwrite(const char* name, 
                           const cgns::ArbitraryGridMotionType_t& atype = cgns::DeformingGrid);

  // -------------------------------------------------------------
  // Links
  // -------------------------------------------------------------

  /// Is the current node a link?
  int islink(void);

  /// Read link (in current node)
  void linkread(std::string& fname, std::string& pname);

  /// Write link (in current node)
  void linkwrite(const char* nname, const char* fname, const char* pname);

};



#endif
