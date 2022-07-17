// -------------------------------------------------------------
// file: mass2solution.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created December 11, 2015 by William A. Perkins
// Last Change: 2017-11-21 09:55:39 d3g096
// -------------------------------------------------------------

#include <cmath>
#include <boost/format.hpp>

#include "mass2solution.h"

// -------------------------------------------------------------
//  class MASS2Solution
// -------------------------------------------------------------

// -------------------------------------------------------------
// MASS2Solution:: constructors / destructor
// -------------------------------------------------------------
MASS2Solution::MASS2Solution(void)
  : CGNSFile()
{
}

MASS2Solution::MASS2Solution(const std::string& name, const int& m)
  : CGNSFile()
{
  try {
    open(name, m);
  } catch (CGNSFile::error& e) {
    throw e;
  }
}

MASS2Solution::~MASS2Solution(void)
{
}

// -------------------------------------------------------------
// MASS2Solution::my_check
// -------------------------------------------------------------
bool
MASS2Solution::my_check(void) const
{
  bool ok(true);
  // check for 2D zones
  // check for required discrete data
  // check for corretly named solutions?
  // check for required solution fields?
  return ok;
}

// -------------------------------------------------------------
// MASS2Solution::my_node_shape
// -------------------------------------------------------------
MASS2Solution::Shape
MASS2Solution::my_node_shape(void) const
{
  Shape result;
  CGNSFile::ZonePtr z(this->zone());
  result[1] = z->size[0][0];
  result[0] = z->size[0][1];
  return result;
}

// -------------------------------------------------------------
// MASS2Solution::my_cell_shape
// -------------------------------------------------------------
MASS2Solution::Shape
MASS2Solution::my_cell_shape(void) const
{
  Shape result;
  CGNSFile::ZonePtr z(this->zone());
  result[1] = z->size[1][0];
  result[0] = z->size[1][1];
  return result;
}


// -------------------------------------------------------------
// MASS2Solution::my_node_field
// -------------------------------------------------------------
MASS2Solution::NodeField *
MASS2Solution::my_node_field(void) const
{
  NodeField *result = new NodeField(my_node_shape());
  return result;
}

// -------------------------------------------------------------
// MASS2Solution::my_cell_field
// -------------------------------------------------------------
MASS2Solution::CellField *
MASS2Solution::my_cell_field(void) const
{
  CellField *result = new CellField(my_cell_shape());
  return result;
}

// -------------------------------------------------------------
// MASS2Solution::open
// -------------------------------------------------------------
void
MASS2Solution::open(const std::string& name, const int& m)
{
  try {
    CGNSFile::open(name, m);
    if (!my_check()) {
      std::string msg(name);
      msg += ": not a MASS2 solution CGNS file";
      throw error(msg);
    }
  } catch (const error& e) {
    throw e;
  }
}

// -------------------------------------------------------------
// MASS2Solution::cell_based
// -------------------------------------------------------------
bool
MASS2Solution::cell_based(const int& sidx) const
{
  ZonePtr z(zone());
  if (nsols() <= 0) {
    throw error("current zone has no solutions");
  }
  cgns::GridLocation_t l(solloc(sidx));
  return (l == cgns::CellCenter);
}

// -------------------------------------------------------------
// MASS2Solution::get_coordinates
// -------------------------------------------------------------
MASS2Solution::NodeVectorFieldPtr
MASS2Solution::get_coordinates(void)
{
  boost::shared_ptr<NodeField> xc(my_node_field());
  coordread(1, xc->data());
  boost::shared_ptr<NodeField> yc(my_node_field());
  coordread(2, yc->data());
  NodeVectorFieldPtr result(new NodeVectorField(xc->shape()));

  *result = blitz::zip(*xc, *yc, Vector());


  // NodeField::iterator ix(xc->begin()), iy(yc->begin());
  // NodeVectorField::iterator i(result->begin());

  // for (; i != result->end(); ++i, ++ix, ++iy) {
  //   Vector v(*ix, *iy);
  //   *i = v;
  // }
  
  return result;
} 

 

// -------------------------------------------------------------
// MASS2Solution::get_cell_coordinates
// -------------------------------------------------------------
BZ_DECLARE_STENCIL2(cell_node_avg, C, N)
  C = 0.25*(N(0,0) + N(0,1) + N(1,0) + N(1,1));
BZ_END_STENCIL

MASS2Solution::CellVectorFieldPtr
MASS2Solution::get_cell_coordinates(void)
{
  NodeVectorFieldPtr nc(get_coordinates());
  // NodeFieldPtr nx(my_node_field()), ny(my_node_field());
  // *nx = nc->extractComponent(double(), 0, 2);
  // *ny = nc->extractComponent(double(), 1, 2);
  // CellFieldPtr cx(my_cell_field());
  CellVectorFieldPtr cc(new CellVectorField(my_cell_shape()));

  for (int i = 0; i < cc->shape()[0]; ++i) {
    for (int j = 0; j < cc->shape()[1]; ++j) {
      (*cc)(i,j) = 0.25*((*nc)(i,j) + (*nc)(i+1,j) + 
                         (*nc)(i+1,j+1) + (*nc)(i,j+1));
    }
  }
                                            
  // blitz::applyStencil(cell_node_avg(), *cx, *nx);  
  // blitz::applyStencil(cell_node_avg(), *cy, *ny);  
  // *cc = blitz::zip(*cx, *cy, Vector());
  
  return cc;
}
  

// -------------------------------------------------------------
// MASS2Solution::get_cell_field
// -------------------------------------------------------------
MASS2Solution::CellFieldPtr 
MASS2Solution::get_cell_field(const int& sidx, const std::string& fname)
{
  CellFieldPtr result;
  try {
    CGNSFile::ZonePtr z(this->zone());
    if (!cell_based(sidx)) {
      std::string msg = 
        boost::str(boost::format("zone %d, solution %d not cell-centered") % 
                   z->index % sidx);
      throw error(msg);
    }
    result.reset(my_cell_field());
    this->fieldread(sidx, fname.c_str(), result->data());
  } catch (const error& e) {
    throw e;
  }
  return result;
}

// -------------------------------------------------------------
// MASS2Solution::put_cell_field
// -------------------------------------------------------------
void
MASS2Solution::put_cell_field(const int& sidx, const std::string& fname,
                              MASS2Solution::CellFieldPtr farray)
{
  try {
    CGNSFile::ZonePtr z(this->zone());
    if (!cell_based(sidx)) {
      std::string msg = 
        boost::str(boost::format("zone %d, solution %d not cell-centered") % 
                   z->index % sidx);
      throw error(msg);
    }
    this->fieldwrite(sidx, fname.c_str(), farray->data());
  } catch (const error& e) {
    throw e;
  }
}
  

// -------------------------------------------------------------
// MASS2Solution::get_cell_velocity_magnitude
// -------------------------------------------------------------
MASS2Solution::CellFieldPtr
MASS2Solution::get_cell_velocity_magnitude(const int& sidx) 
{
  CellFieldPtr vx(get_cell_field(sidx, "VelocityX"));
  CellFieldPtr vy(get_cell_field(sidx, "VelocityY"));
  CellFieldPtr vmag(my_cell_field());
  
  *vmag = (*vx)*(*vx);
  *vmag += (*vy)*(*vy);
  *vmag = sqrt(*vmag);
  return vmag;
}
  
// -------------------------------------------------------------
// MASS2Solution::get_velocity
// -------------------------------------------------------------
MASS2Solution::CellVectorFieldPtr
MASS2Solution::get_cell_velocity(const int& sidx) 
{
  CellFieldPtr vx(get_cell_field(sidx, "VelocityX"));
  CellFieldPtr vy(get_cell_field(sidx, "VelocityY"));
  CellVectorFieldPtr result(new CellVectorField(vx->shape()));

  *result = blitz::zip(*vx, *vy, Vector());

  // CellField::iterator ix(vx->begin()), iy(vy->begin());
  // CellVectorField::iterator i(result->begin());
  // for (; i != result->end(); ++i, ++ix, ++iy) {
  //   Vector v(*ix, *iy);
  //   *i = v;
  // }
  return result;
}
  
// -------------------------------------------------------------
// MASS2Solution::get_shear_velocity
// -------------------------------------------------------------
MASS2Solution::CellFieldPtr
MASS2Solution::get_cell_shear_velocity(const int& sidx) 
{
  const double density(1.936);  // slug/ft3
  CellFieldPtr result(get_cell_field(sidx, "shear")); // Units lbf/ft2
  *result /= density;
  *result = sqrt(*result);      // should be ft/s
  return result;
}
    
