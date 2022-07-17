// Emacs Mode Line: -*- Mode:c++;-*-
// -------------------------------------------------------------
// file: mass2series.h
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created December 28, 2015 by William A. Perkins
// Last Change: 2018-06-08 09:45:21 d3g096
// -------------------------------------------------------------


#ifndef _mass2series_h_
#define _mass2series_h_

#include <boost/utility.hpp>
#include <boost/scoped_ptr.hpp>
#include <string>
#include <vector>

#include "mass2solution.h"

// -------------------------------------------------------------
//  class MASS2Series
// -------------------------------------------------------------
class MASS2Series 
  : private boost::noncopyable
{
protected:

  /// List of solution files
  std::vector<std::string> my_files;

  /// (optional) List of solution indexes
  std::vector<int> my_indexes;

  /// The base (in all files) to work with
  int my_base;

  /// The zone (in all files) to work with
  int my_zone;

  /// The current solution index (within current file)
  int my_current;

  /// The current solution file
  boost::scoped_ptr<MASS2Solution> my_current_file;

  /// The current solution index in @c my_current_file
  int my_current_sindex;

  /// Read a list of files and solution indexes from a named file
  void my_read_list(const std::string& name);

public:

  /// Construct with a list of files (use all solutions)
  MASS2Series(const std::vector<std::string>& filelist);

  /// Construct with the name of a file that contains a file/solution list
  MASS2Series(const std::string& listname);

  /// Destructor
  ~MASS2Series(void);

  /// Advance to the next solution (true returned if success)
  bool advance(void);

  /// Set the base to work with
  void base(const int& b);

  /// Set the zone to work with
  void zone(const int& z);

  /// Get the current file
  const MASS2Solution *current(void) const;

  /// Get the name of the current file
  std::string current_name(void) const;

  /// Get the current solution index
  int current_sindex(void) const;

  /// Get the current solution name
  std::string current_sname(void) const;

  /// Get node coordinates frome the current solution file
  MASS2Solution::NodeVectorFieldPtr get_coordinates(void);

  /// Get cell centroid coordinates
  MASS2Solution::CellVectorFieldPtr get_cell_coordinates(void);

  /// Is the solution (if any) cell-based?
  bool cell_based(void) const;

  /// Get a (cell-based) solution field
  MASS2Solution::CellFieldPtr get_cell_field(const std::string& fname);

  /// Get the (cell-based) velocity magnitude field
  MASS2Solution::CellFieldPtr get_cell_velocity_magnitude(void);

  /// Get the (cell-based) velocity vector
  MASS2Solution::CellVectorFieldPtr get_cell_velocity(void);

  /// Get the (cell-based) shear velocity
  MASS2Solution::CellFieldPtr get_cell_shear_velocity(void);
};



#endif
