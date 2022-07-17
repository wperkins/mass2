// Emacs Mode Line: -*- Mode:c++;-*-
// -------------------------------------------------------------
// file: mass2solution.h
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created December 11, 2015 by William A. Perkins
// Last Change: 2019-02-20 10:32:16 d3g096
// -------------------------------------------------------------


#ifndef _mass2solution_h_
#define _mass2solution_h_

#include <boost/shared_ptr.hpp>
#include <blitz/blitz.h>
#include <blitz/array.h>

#include "cgnsfile.h"

// -------------------------------------------------------------
//  class MASS2Solution
// -------------------------------------------------------------
class MASS2Solution 
  : public CGNSFile
{
public:

  typedef blitz::Array<double, 2> CellField;
  typedef blitz::Array<double, 2> NodeField;
  typedef boost::shared_ptr<NodeField> NodeFieldPtr;
  typedef boost::shared_ptr<CellField> CellFieldPtr;

  typedef blitz::TinyVector<double, 2> Vector;
  typedef blitz::Array<Vector, 2> CellVectorField;
  typedef blitz::Array<Vector, 2> NodeVectorField;
  typedef boost::shared_ptr<CellVectorField> CellVectorFieldPtr;
  typedef boost::shared_ptr<NodeVectorField> NodeVectorFieldPtr;

  typedef blitz::TinyVector<int, 2> Shape;
  
protected:

  /// Make sure this is a valid MASS2 file
  bool my_check(void) const;

  /// The shape of a node-based array
  Shape my_node_shape(void) const;

  /// The shape of a cell-based array
  Shape my_cell_shape(void) const;

  /// Make an empty field array with node dimensions
  NodeField *my_node_field(void) const;

  /// Make an empty cell array with cell dimensions
  CellField *my_cell_field(void) const;
  
  
public:

  /// Default constructor.
  MASS2Solution(void);

  /// Construct and open file
  MASS2Solution(const std::string& name, const int& m = CG_MODE_READ);

  /// Destructor
  ~MASS2Solution(void);

  /// Open the specified file and prepare it for writing.
  void open(const std::string& name, const int& m = CG_MODE_READ);

  /// Get an empty (node-based) array
  NodeFieldPtr node_scalar_array(void) const
  {
    NodeFieldPtr result(my_node_field());
    return result;
  }

  /// Get an empty (cell-based) array
  CellFieldPtr cell_scalar_array(void) const
  {
    CellFieldPtr result(my_cell_field());
    return result;
  }

  /// Get node coordinates
  NodeVectorFieldPtr get_coordinates(void);

  /// Get cell centroid coordinates
  CellVectorFieldPtr get_cell_coordinates(void);

  /// Is the solution (if any) cell-based?
  bool cell_based(const int& sidx = 1) const;

  /// Get a (cell-based) solution field
  CellFieldPtr get_cell_field(const int& sidx, const std::string& fname);

  /// Put a (cell-based) solution field
  void put_cell_field(const int& sidx, const std::string& fname, 
                      CellFieldPtr values);

  /// Get the (cell-based) velocity magnitude field
  CellFieldPtr get_cell_velocity_magnitude(const int& sidx);

  /// Get the (cell-based) velocity vector
  CellVectorFieldPtr get_cell_velocity(const int& sidx);

  /// Get the (cell-based) shear velocity
  CellFieldPtr get_cell_shear_velocity(const int& sidx);

};

#endif
