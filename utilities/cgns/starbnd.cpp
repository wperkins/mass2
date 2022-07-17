// -------------------------------------------------------------
/**
 * @file   starbnd.cpp
 * @author William A. Perkins
 * @date Fri Dec  2 14:44:59 2011
 * 
 * @brief  A program to add boundary information to StarCD CGNS output.
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
// Last Change: 2014-10-14 10:13:43 d3g096
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

/// A place to store a boundary face
struct bndface {
  int starid;
  int vidx[4];
  int region;
  int rad;
  cgns::BCType_t btype;
};

/// A bag to hold a bunch of bndface
typedef std::vector<bndface> bndfacelist;

// -------------------------------------------------------------
// read_star_bound
// -------------------------------------------------------------
int
read_star3_bound(std::istream& f, bndfacelist& blist)
{
  using namespace boost::spirit::classic;

  bndface tmp;

  rule <phrase_scanner_t> bound_rule = 
    int_p[assign_a(tmp.starid)] >>
    int_p[assign_a(tmp.vidx[0])] >>
    int_p[assign_a(tmp.vidx[1])] >>
    int_p[assign_a(tmp.vidx[2])] >>
    int_p[assign_a(tmp.vidx[3])] >>
    int_p[assign_a(tmp.region)] >>
    int_p[assign_a(tmp.rad)] >>
    ( str_p("INLE")[assign_a(tmp.btype,cgns::BCInflow)] |
      str_p("CYCL")[assign_a(tmp.btype,cgns::BCInflow)] |
      str_p("WALL")[assign_a(tmp.btype,cgns::BCWall)] |
      str_p("OUTL")[assign_a(tmp.btype,cgns::BCOutflow)] |
      str_p("PRES")[assign_a(tmp.btype,cgns::BCExtrapolate)] |
      str_p("SYMP")[assign_a(tmp.btype,cgns::BCSymmetryPlane)] ) >>
    end_p;

  char cbuf[1024];
  int lnum = 0;
  int bnum = 0;
  int ierr = 0;

  while (f) {
    f.getline(cbuf, sizeof(cbuf));
    lnum++;

    parse_info<const char *> presult;

    // skip blank lines
    presult = parse(cbuf, end_p, space_p);
    if (presult.full) continue;

    presult = parse(cbuf, bound_rule, space_p);
    if (presult.full) {
      bnum++;
      blist.push_back(tmp);
    } else {
      ierr++;
      std::cerr << "line " << lnum << ": parse error" << std::endl;
    }
  }
  
  return ierr;
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
    ("cgns", po::value< std::string >(), "CGNS file to modify")
    ("bnd", po::value< std::string >(), "ProStar boundary export file")
    ;

  po::positional_options_description p;
  p.add("cgns", 1);
  p.add("bnd", 2);

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

  if (vm.count("bnd") <= 0) {
    std::cerr << program << ": error: missing boundary file" << std::endl;
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << "\n";
    return 1;
  }
  int base = vm["base"].as<int>();
  int zone = vm["zone"].as<int>();;
  std::string cgnsname(vm["cgns"].as<std::string>());
  std::string bndname(vm["bnd"].as<std::string>());

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

  std::ifstream bnd(bndname.c_str());

  if (!bnd) {
    std::cerr << program << ": error: cannot open file \"" 
              << bndname << "\" for reading" << std::endl;
    exit(3);
  }

  bndfacelist blist;

  if (read_star3_bound(bnd, blist)) {
    std::cerr << "too many errors" << std::endl;
  }
  bnd.close();
        
  std::cerr << program << ": info: Read " << blist.size() 
            << " boundary faces" << std::endl;

  std::set<int> regions;
  std::map<int, cgns::BCType_t> btypes;

  for (bndfacelist::const_iterator i = blist.begin(); i != blist.end(); i++) {
    regions.insert(i->region);
    btypes[i->region] = i->btype;
  }

  std::cerr << program << ": info: Found " 
            << regions.size() << " boundary regions" << std::endl;
  
  try {
    for (std::set<int>::const_iterator ireg = regions.begin(); ireg != regions.end(); ireg++) {
      std::set<int> vidx;
      for (bndfacelist::const_iterator i = blist.begin(); i != blist.end(); i++) {
        if (*ireg == i->region) {
          for (int j = 0; j < 4; j++) {
            vidx.insert(i->vidx[j]);
          }
        }
      }
      std::ostringstream bname;
      bname << "StarBnd" << std::setw(3) << std::setfill('0') << *ireg << std::ends;

      std::vector<cgns::cgsize_t> v;
      v.reserve(vidx.size());
      std::copy(vidx.begin(), vidx.end(), back_inserter(v));
      CGNSFile::BCPtr bc(cgns.newbc());
      bc->name = bname.str();
      bc->type = btypes[*ireg];
      bc->npts = vidx.size();
      cgns.bcwrite(bc, &v[0]);
    }
    cgns.close();
  } catch (std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }
}


