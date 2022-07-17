// -------------------------------------------------------------
/**
 * @file   cgnsbc.cpp
 * @author William A. Perkins
 * @date Fri Dec  2 14:45:00 2011
 * 
 * @brief  A program to put a boundary condition on a CGNS zone
 * 
 * 
 */
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created August 18, 2010 by William A. Perkins
// Last Change: 2014-10-14 10:12:32 d3g096
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <string>
#include <boost/spirit/include/classic.hpp>
#include <set>
#include <map>
#include <libgen.h>
#include <boost/program_options.hpp>
#include <boost/algorithm/string.hpp>

#include "cgnsfile.h"


/// The name of this program
std::string program;

/// A bag to put vertex indexes in
typedef std::set<cgns::cgsize_t> iset;

/// A vector to put vertex indexes in
typedef std::vector<cgns::cgsize_t> ivector;

// -------------------------------------------------------------
// write_boundary
// -------------------------------------------------------------
void
write_boundary(CGNSFile& cgns, 
               const std::string& bname, 
               const cgns::BCType_t& bctype, 
               ivector& vidx)
{
  int nvert(vidx.size());
  CGNSFile::BCPtr bc(cgns.newbc());
  if (bc->zone->type == cgns::Structured) {
    nvert /= 3;
  }
  bc->name = bname;
  bc->type = bctype;
  bc->npts = nvert;
  cgns.bcwrite(bc, &vidx[0]);
}

// -------------------------------------------------------------
// write_boundary
// -------------------------------------------------------------
void
write_boundary(CGNSFile& cgns, 
               const std::string& bname, 
               const cgns::BCType_t& bctype, 
               iset& vidx)
{
  ivector v;
  v.reserve(vidx.size());
  std::copy(vidx.begin(), vidx.end(), back_inserter(v));
  write_boundary(cgns, bname, bctype, v);
}

// -------------------------------------------------------------
// write_cyclic
// -------------------------------------------------------------
void
write_cyclic(CGNSFile& cgns, 
             const std::string& bname, 
             const std::string& czonename, 
             const std::string& dconname,
             const std::vector<double> translation,
             ivector& vert)
{
  int nvert(vert.size());
  
  CGNSFile::ConnPtr conn = cgns.newconn();
  if (conn->zone->type == cgns::Structured) {
    nvert /= 3;
  }
  
  conn->name = bname;
  conn->conntype = cgns::Abutting1to1;
  conn->location = cgns::Vertex;
  conn->donorname = czonename;
  conn->donorconnname = dconname;
  conn->ptsettype = cgns::PointList;
  conn->npnts = vert.size();
  conn->ndata_donor = 0;
  conn->periodic = true;
  for (int i = 0; i < std::min<int>(3, translation.size()); i++) {
    conn->periodic_tranlation[i] = translation[i];
  }

  cgns.connwrite(conn, &vert[0], NULL, NULL);
}


// -------------------------------------------------------------
// write_cyclic
// -------------------------------------------------------------
void
write_cyclic(CGNSFile& cgns, 
             const std::string& bname, 
             const std::string& czonename, 
             const std::string& dconname,
             const std::vector<double> translation,
             iset& vidx)
{
  ivector vert;
  vert.reserve(vidx.size());
  std::copy(vidx.begin(), vidx.end(), back_inserter(vert));
  write_cyclic(cgns, bname, czonename, dconname, translation, vert);
}


// -------------------------------------------------------------
// read_star_vrt
// -------------------------------------------------------------
int
read_star3_vrt(std::istream& f, iset& vlist)
{
  using namespace boost::spirit::classic;

  int vid;
  double x, y, z;

  rule <phrase_scanner_t> vertex_rule = 
    int_p[assign_a(vid)] >>
    real_p[assign_a(x)] >>
    real_p[assign_a(y)] >>
    real_p[assign_a(z)] >>
    end_p;

  char cbuf[1024];
  int lnum = 0;
  int ierr = 0;

  // skip the header (2 lines)
  f.getline(cbuf, sizeof(cbuf));
  f.getline(cbuf, sizeof(cbuf));

  while (f) {
    f.getline(cbuf, sizeof(cbuf));
    lnum++;

    parse_info<const char *> presult;

    // skip blank lines
    presult = parse(cbuf, end_p, space_p);
    if (presult.full) continue;

    presult = parse(cbuf, vertex_rule, space_p);
    if (presult.full) {
      vlist.insert(vid);
    } else {
      ierr++;
      std::cerr << "line " << lnum << ": parse error" << std::endl;
    }
  }
  
  return ierr;
}

// -------------------------------------------------------------
// bc_type_by_name
// -------------------------------------------------------------
cgns::BCType_t
bc_type_by_name(const std::string& n)
{
  cgns::BCType_t result = cgns::BCTypeNull;
  for (int i = 0; i < NofValidBCTypes; i++) {
    if (n == cgns::BCTypeName[i]) {
      result = static_cast<cgns::BCType_t>(i);
    }
  }
  return result;
}

// -------------------------------------------------------------
// struct Side
// -------------------------------------------------------------
struct Side {
  enum Face { West, East, South, North, Bottom, Top };
  Face face;
  Side(const Face& f) : face(f) {};
};

void 
validate(boost::any& v, 
         const std::vector<std::string>& values,
         Side* target_type, int i)
{
  using namespace boost::program_options;

  // Make sure no previous assignment to 'a' was made.
  validators::check_first_occurrence(v);

  // Extract the first string from 'values'. If there is more than
  // one string, it's an error, and exception will be thrown.
  std::string s =
    boost::to_lower_copy(validators::get_single_string(values));  
  
  Side::Face face;
  if (s == "west") {
    face = Side::West;
  } else if (s == "east") {
    face = Side::East;
  } else if (s == "south") {
    face = Side::South;
  } else if (s == "north") {
    face = Side::North;
  } else if (s == "bottom") {
    face = Side::Bottom;
  } else if (s == "top") {
    face = Side::Top;
  } else {
#if BOOST_VERSION >= 104200
    throw validation_error(validation_error::invalid_option_value, 
                           "must be one of West, East, South, North, Bottom, Top");
#else
    throw validation_error("must be one of West, East, South, North, Bottom, Top");
#endif
  }
  v = boost::any(Side(face));
}

// -------------------------------------------------------------
// side_indexes
// -------------------------------------------------------------
void
side_indexes(const int& isize, const int& jsize, const int& ksize,
             const Side::Face& theside, ivector& vidx)
{
  int n(0);
  int istart(1), iend(isize);
  int jstart(1), jend(jsize);
  int kstart(1), kend(ksize);
  switch (theside) {
  case (Side::West):
    n = jsize*ksize;
    iend = 1;
    break;
  case (Side::East):
    n = jsize*ksize;
    istart = isize;
    break;
  case (Side::South):
    n = isize*ksize;
    jend = 1;
    break;
  case (Side::North):
    n = isize*ksize;
    jstart = jsize;
    break;
  case (Side::Bottom):
    n = isize*jsize;
    kend = 1;
    break;
  case (Side::Top):
    n = isize*jsize;
    kstart = kend;
    break;
  }

  vidx.reserve(3*n);
  for (int i = istart; i <= iend; i++) {
    for (int j = jstart; j <= jend; j++) {
      for (int k = kstart; k <= kend; k++) {
        vidx.push_back(i);
        vidx.push_back(j);
        vidx.push_back(k);
      }
    }
  }

  return;
}



// -------------------------------------------------------------
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{

  program = basename(argv[0]);

  namespace po = boost::program_options;

  int base, zoneidx;
  std::string cgnsname;
  std::string vertname;
  std::string bname;

  bool docyclic(false);
  std::vector<double> cyclictrans(3);

  cgns::BCType_t btype;
  std::string dconnname;

  Side::Face theside;
  bool dostructured(false);



                                // Parse the command line options

  po::options_description desc("Available options");
  try {
    desc.add_options()
      ("help", "produce this help message")
      ("cgns", po::value< std::string >(), "CGNS file to modify")
      ("base", po::value<int>()->default_value(1), "CGNS base node index")
      ("zone", po::value<int>()->default_value(1), "zone index")
      ("name", po::value< std::string >(), "Boundary name")
      ("type", po::value< std::string >()->default_value(cgns::BCTypeName[cgns::BCTypeUserDefined]), "CGNS boundary type name")
      ("cyclic", po::value< std::string >(), "make a cyclic connection, not a boundary")
      ("translate-x", po::value< double >()->default_value(0.0), "cyclic translation in the x-direction")
      ("translate-y", po::value< double >()->default_value(0.0), "cyclic translation in the y-direction")
      ("translate-z", po::value< double >()->default_value(0.0), "cyclic translation in the z-direction")
      ("vert", po::value< std::string >(), "ProStar vertex export file (unstructured only)")
      ("side", po::value< Side >(), "make boundary on the specified side (structured only)")
      ;
       
    po::positional_options_description p;
    p.add("cgns", 1);
    
    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
    po::notify(vm);    
    
    if (vm.count("help")) {
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << "\n";
      return 1;
    }
    
    if (vm.count("cgns") <= 0) {
      std::cerr << program << ": error: missing cgns file" << std::endl;
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << "\n";
      return 1;
    }
    
    if (vm.count("name") <= 0) {
      std::cerr << program << ": error: missing boundary name" << std::endl;
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << "\n";
      return 1;
    }
    
    base = vm["base"].as<int>();
    zoneidx = vm["zone"].as<int>();;

    if (vm.count("vert") > 0) {
      vertname = (vm["vert"].as<std::string>());
    }

    cgnsname = (vm["cgns"].as<std::string>());
    bname = (vm["name"].as<std::string>());

    docyclic = (vm.count("cyclic") > 0);
    cyclictrans[0] = vm["translate-x"].as<double>();
    cyclictrans[1] = vm["translate-y"].as<double>();
    cyclictrans[2] = vm["translate-z"].as<double>();

    std::string btypename(vm["type"].as<std::string>());
    btype = bc_type_by_name(btypename);
    if (!docyclic && 
        btype == cgns::BCTypeNull && 
        btypename != cgns::BCTypeName[cgns::BCTypeNull]) {
      std::cerr << program << ": error: unknown boundary type: \"" 
                << btypename << "\"" << std::endl;
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << "\n";
      return 1;
    } 

    if (docyclic) {
      dconnname = vm["cyclic"].as<std::string>();
    }

    if (vm.count("side") > 0) {
      Side tmpside(vm["side"].as<Side>());
      theside = tmpside.face;
      dostructured = true;
    }

  } catch (po::error& e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << "\n";
    return(3);
  } catch (boost::bad_any_cast& e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    return(3);
  } catch (std::runtime_error& e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    return(3);
  }

                                // Check the CGNS file 

  CGNSFile cgns;
  int isize, jsize, ksize;
  try {
    cgns.open(cgnsname.c_str(), CG_MODE_MODIFY);
    cgns.base(base);
    CGNSFile::ZonePtr zone(cgns.zoneread(zoneidx));

    switch (zone->type) {
    case (cgns::Structured):
      if (!dostructured) {
        std::cerr << program << ": error: " << cgnsname 
                  << ": no useful options set for structured zone " << zone->name 
                  << std::endl;
        return (3);
      }

      isize = zone->size[0][0];
      jsize = zone->size[0][1];
      ksize = zone->size[0][2];

      std::cerr << program << ": info: structured zone (" 
                << isize << ", " << jsize << ", " << ksize << ")"
                << std::endl;
      break;
    case (cgns::Unstructured):
      if (dostructured) {
        std::cerr << program << ": error: " << cgnsname 
                  << ": incompatible option(s) set for unstructured zone " << zone->name 
                  << std::endl;
        return (3);
      }

      if (vertname.empty()) {
        std::cerr << program << ": error: " << cgnsname 
                  << ": nothing to do for unstructured zone " << zone->name 
                  << std::endl;
        return (3);
      }
      break;
    default:
      std::cerr << program << ": error: " << cgnsname 
                << ": zone " << zone->name << " has unknown type"
                << std::endl;
      return 3;
      break;
    }
  } catch (std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }

  ivector vidx;

  if (!vertname.empty()) {

    // Open and read the boundary file
    
    std::ifstream vertf(vertname.c_str());
    
    if (!vertf) {
      std::cerr << program << ": error: cannot open file \"" 
                << vertname << "\" for reading" << std::endl;
      exit(3);
    }
    
    iset vset;

    if (read_star3_vrt(vertf, vset)) {
      std::cerr << "too many errors" << std::endl;
    }
    vertf.close();

    vidx.reserve(vset.size());
    std::copy(vset.begin(), vset.end(), back_inserter(vidx));
    
    std::cerr << program << ": info: Read " << vidx.size() 
              << " vertex indexes" << std::endl;
  } 

  if (dostructured) {
    side_indexes(isize, jsize, ksize, theside, vidx);
  }



  try {
    if (docyclic) {
      std::string connzone = cgns.zone()->name;
      write_cyclic(cgns, bname, connzone.c_str(), dconnname, cyclictrans, vidx);
    } else {
      write_boundary(cgns, bname, btype, vidx);
    }
    cgns.close();
  } catch (std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }
}


