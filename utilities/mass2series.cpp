// -------------------------------------------------------------
// file: mass2series.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created December 31, 2015 by William A. Perkins
// Last Change: 2019-02-21 10:44:45 d3g096
// -------------------------------------------------------------

#include <iostream>
#include <fstream>
#include <exception>
// #include <boost/spirit.hpp>
#include <boost/spirit/include/classic.hpp>

#include "mass2series.h"

// -------------------------------------------------------------
//  class MASS2Series
// -------------------------------------------------------------

// -------------------------------------------------------------
// MASS2Series:: constructors / destructor
// -------------------------------------------------------------
MASS2Series::MASS2Series(const std::vector<std::string>& filelist)
  : my_files(filelist), my_indexes(),
    my_base(1), my_zone(1),
    my_current(0), 
    my_current_file(),
    my_current_sindex(1)
{
}

MASS2Series::MASS2Series(const std::string& listname)
  : my_files(), my_indexes(),
    my_base(1), my_zone(1),
    my_current(0), 
    my_current_file(),
    my_current_sindex(1)
{
  my_read_list(listname);
}


MASS2Series::~MASS2Series(void)
{
}

// -------------------------------------------------------------
// MASS2Series::my_read_list
// -------------------------------------------------------------
void 
MASS2Series::my_read_list(const std::string& name)
{
  using namespace boost::spirit;
  using namespace boost::spirit::classic;

  std::ifstream f;

  f.open(name.c_str());
  if (!f) {
    std::string msg(name);
    msg += ": error: cannot open";
    throw std::runtime_error(msg);
  }

  double stime;
  std::string sname;
  int sindex;

  chlit<> dq = ch_p('"');
  chlit<> sq = ch_p('\'');

  // this should match a comment or a blank line
  rule <phrase_scanner_t> comment = ( ( '#' >> *(anychar_p) >> end_p)  || end_p );
    
  // this rule defines the record format
  rule <phrase_scanner_t> fldrule =

                                // solution time, maybe

    !(real_p[assign_a(stime)]) >>

                                // CGNS file name (may or may not be quoted)
    (
     (lexeme_d[ dq >> (*(~dq))[assign_a(sname)] >> dq ]) ||
     (lexeme_d[ sq >> (*(~sq))[assign_a(sname)] >> sq ]) ||
     (lexeme_d[+graph_p[push_back_a(sname)]] ) 
     ) >>

                                // A solution/state index or
                                // name (which MUST be quoted)

    (
      int_p[assign_a(sindex)] 
     ) >>

                                // there may be a comment or just the end

    ( ( '#' >> *anychar_p >> end_p)  || end_p );

  int ierr = 0;
  int lnum = 0;
  char cbuf[1024];

  while (f) {

    f.getline(cbuf, sizeof(cbuf));
    lnum++;

    parse_info<const char *> presult;

    //  initialize
    sname.clear();
    sindex = -1;

    // check for comments first, if it matches go to the next line
    presult = parse(cbuf, comment, space_p);
    if (presult.full) continue;

    // this should be a non-empty line containing all necessary info,
    // so put it in the list
    presult = parse(cbuf, fldrule, space_p);

    if (!presult.hit) {
      std::cerr << name << ": error, line " << lnum << ": cannot parse" << std::endl;
      ierr++;
      continue;
    } else {
      if (sindex <= 0) {
        std::cerr << name << ": error, line " << lnum 
                  << ": bad solution index (" << sindex << ")"
                  << std::endl;
        ierr++;
      } else {
        my_files.push_back(sname);
        my_indexes.push_back(sindex);
      }

      if (!presult.full) {
        std::cerr << name << ": warning, line " << lnum 
                  << ": ignoring from \"" 
                  << presult.stop << "\"" << std::endl;
      }
    }
  }

  f.close();

  if (ierr) {
    std::string msg(name);
    msg += ": error: too many errors";
    throw std::runtime_error(msg);
  }
}

// -------------------------------------------------------------
// MASS2Series::advance
// -------------------------------------------------------------
bool
MASS2Series::advance(void)
{
  bool donew(false);

  if (!my_current_file) {
    my_current = 0;
    donew = true;
  } else if (!my_indexes.empty()) {
    my_current += 1;
    donew = (my_current >= my_files.size() ||
             (my_files[my_current - 1] != my_files[my_current]));
    if (!donew) {
      my_current_sindex = my_indexes[my_current];
    }
  } else {
    my_current_sindex += 1;
    if (my_current_sindex > my_current_file->nsols()) {
      my_current += 1;
      donew = true;
    }
  } 
  if (donew) {
    if (my_current >= my_files.size()) {
      return false;
    }

    my_current_file.reset(new MASS2Solution(my_files[my_current]));
    my_current_file->base(my_base);
    my_current_file->zone(my_zone);
    if (!my_indexes.empty()) {
      my_current_sindex = my_indexes[my_current];
    } else {
      my_current_sindex = 1;
    }
  }  
  return true;
}

// -------------------------------------------------------------
// MASS2Series::base
// -------------------------------------------------------------
void
MASS2Series::base(const int& b) 
{
  my_base = b;
  if (my_current_file) {
    my_current_file->base(my_base);
  }
}

// -------------------------------------------------------------
// MASS2Series::zone
// -------------------------------------------------------------
void
MASS2Series::zone(const int& z)
{
  my_zone = z;
  if (my_current_file) {
    my_current_file->zone(my_zone);
  }
}

// -------------------------------------------------------------
// MASS2Series::current
// -------------------------------------------------------------
const MASS2Solution *
MASS2Series::current(void) const
{
  return my_current_file.get();
}

// -------------------------------------------------------------
// MASS2Series::current_name
// -------------------------------------------------------------
std::string
MASS2Series::current_name(void) const
{
  return my_current_file->name();
}

// -------------------------------------------------------------
// MASS2Series::current_sindex
// -------------------------------------------------------------
int
MASS2Series::current_sindex(void) const
{
  return my_current_sindex;
}

// -------------------------------------------------------------
// MASS2Series::current_sname
// -------------------------------------------------------------
std::string
MASS2Series::current_sname(void) const
{
  return my_current_file->solinfo(my_current_sindex);
}

// -------------------------------------------------------------
// MASS2Series::get_coordinates
// -------------------------------------------------------------
MASS2Solution::NodeVectorFieldPtr 
MASS2Series::get_coordinates(void)
{
  return my_current_file->get_coordinates();
}

// -------------------------------------------------------------
// MASS2Series::get_cell_coordinates
// -------------------------------------------------------------
MASS2Solution::CellVectorFieldPtr 
MASS2Series::get_cell_coordinates(void)
{
  return my_current_file->get_cell_coordinates();
}

// -------------------------------------------------------------
// MASS2Series::cell_based
// -------------------------------------------------------------
bool
MASS2Series::cell_based(void) const
{
  return my_current_file->cell_based(my_current_sindex);
}

// -------------------------------------------------------------
// MASS2Series::get_cell_field
// -------------------------------------------------------------
MASS2Solution::CellFieldPtr 
MASS2Series::get_cell_field(const std::string& fname)
{
  return my_current_file->get_cell_field(my_current_sindex, fname);
}

// -------------------------------------------------------------
// MASS2Series::get_cell_velocity_magnitude
// -------------------------------------------------------------
MASS2Solution::CellFieldPtr 
MASS2Series::get_cell_velocity_magnitude(void)
{
  return my_current_file->get_cell_velocity_magnitude(my_current_sindex);
}

// -------------------------------------------------------------
// MASS2Series::get_cell_velocity
// -------------------------------------------------------------
MASS2Solution::CellVectorFieldPtr 
MASS2Series::get_cell_velocity(void)
{
  return my_current_file->get_cell_velocity(my_current_sindex);
}

// -------------------------------------------------------------
// MASS2Series::get_cell_shear_velocity
// -------------------------------------------------------------
MASS2Solution::CellFieldPtr 
MASS2Series::get_cell_shear_velocity(void)
{
  return my_current_file->get_cell_shear_velocity(my_current_sindex);
}




