// -------------------------------------------------------------
/**
 * @file   starvrt.cpp
 * @author William A. Perkins
 * @date Fri Dec  2 14:44:59 2011
 * 
 * @brief  A program to add a boundary to a CGNS file given a Star vertex file.
 * 
 * 
 */
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created December 22, 2005 by William A. Perkins
// Last Change: 2014-10-15 07:17:28 d3g096
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
typedef std::set<int> iset;

// -------------------------------------------------------------
// write_boundary
// -------------------------------------------------------------
void
write_boundary(CGNSFile& cgns, 
               const std::string& bname, 
               const cgns::BCType_t& bctype, 
               iset& vidx)
{
  std::vector<cgns::cgsize_t> v;
  v.reserve(vidx.size());
  std::copy(vidx.begin(), vidx.end(), back_inserter(v));
  CGNSFile::BCPtr bc(cgns.newbc());
  bc->name = bname;
  bc->type = bctype;
  bc->npts = vidx.size();
  cgns.bcwrite(bc, &v[0]);
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
  std::vector<cgns::cgsize_t> vert;
  vert.reserve(vidx.size());
  std::copy(vidx.begin(), vidx.end(), back_inserter(vert));
  
  CGNSFile::ConnPtr conn = cgns.newconn();
  
  conn->name = bname;
  conn->conntype = cgns::Abutting1to1;
  conn->location = cgns::Vertex;
  conn->donorname = czonename;
  conn->donorconnname = dconname;
  conn->ptsettype = cgns::PointList;
  conn->npnts = vidx.size();
  conn->ndata_donor = 0;
  conn->periodic = true;
  for (int i = 0; i < std::min<int>(3, translation.size()); i++) {
    conn->periodic_tranlation[i] = translation[i];
  }

  cgns.connwrite(conn, &vert[0], NULL, NULL);
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
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{

  program = basename(argv[0]);

  namespace po = boost::program_options;

                                // Parse the command line options

  int opt;
  po::options_description desc("Available options");
  desc.add_options()
    ("help", "produce this help message")
    ("base", po::value<int>(&opt)->default_value(1), "CGNS base node index")
    ("zone", po::value<int>(&opt)->default_value(1), "zone index")
    ("type", 
     po::value< std::string >()->default_value(cgns::BCTypeName[cgns::BCTypeUserDefined]),
     "CGNS boundary type name")
    ("cyclic", po::value< std::string >(), 
     "specified vertexes make up a cyclic connection, not a boundary")
    ("translate-x", po::value< double >()->default_value(0.0), "cyclic translation in the x-direction")
    ("translate-y", po::value< double >()->default_value(0.0), "cyclic translation in the y-direction")
    ("translate-z", po::value< double >()->default_value(0.0), "cyclic translation in the z-direction")
    ("cgns", po::value< std::string >(), "CGNS file to modify")
    ("vert", po::value< std::string >(), "ProStar vertex export file")
    ("name", po::value< std::string >(), "Boundary name")
    ;

  po::positional_options_description p;
  p.add("cgns", 1);
  p.add("vert", 1);
  p.add("name", 1);

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

  if (vm.count("vert") <= 0) {
    std::cerr << program << ": error: missing boundary file" << std::endl;
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

  int base = vm["base"].as<int>();
  int zone = vm["zone"].as<int>();;
  std::string cgnsname(vm["cgns"].as<std::string>());
  std::string vertname(vm["vert"].as<std::string>());
  std::string bname(vm["name"].as<std::string>());

  bool docyclic = (vm.count("cyclic") > 0);
  std::vector<double> cyclictrans(3);
  cyclictrans[0] = vm["translate-x"].as<double>();
  cyclictrans[1] = vm["translate-y"].as<double>();
  cyclictrans[2] = vm["translate-z"].as<double>();

  std::string btypename(vm["type"].as<std::string>());
  cgns::BCType_t btype = bc_type_by_name(btypename);
  std::string dconnname;
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

                                // Check the CGNS file 

  CGNSFile cgns;
  try {
    cgns.open(cgnsname.c_str(), CG_MODE_MODIFY);
    cgns.base(base);
    cgns.zoneread(zone);

    switch (cgns.zone()->type) {
    case (cgns::Structured):
      std::cout << argv[0] << ": error: " << cgnsname 
                << ": unstructured only only" << std::endl;
      break;
    case (cgns::Unstructured):
    default:
      break;
    }
  } catch (std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }
 

                                // Open and read the boundary file

  std::ifstream vertf(vertname.c_str());

  if (!vertf) {
    std::cerr << program << ": error: cannot open file \"" 
              << vertname << "\" for reading" << std::endl;
    exit(3);
  }

  iset vidx;

  if (read_star3_vrt(vertf, vidx)) {
    std::cerr << "too many errors" << std::endl;
  }
  vertf.close();
        
  std::cerr << program << ": info: Read " << vidx.size() 
            << " vertex indexes" << std::endl;

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


