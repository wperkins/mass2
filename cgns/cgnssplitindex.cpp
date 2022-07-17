// -------------------------------------------------------------
// file: cgnssplitindex.cpp
/**
 * @file   cgnssplitindex.cpp
 * @author William A. Perkins
 * @date Thu Jun 28 10:07:19 2012
 * 
 * @brief This program interrogates a CGNS file produced by cgnssplit
 * and prints the original cell(vertex) index given the new zone and
 * cell(vertex) index.
 * 
 * 
 */

// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created March 12, 2009 by William A. Perkins
// Last Change: Thu Jun 28 10:07:19 2012 by William A. Perkins <d3g096@pe10900.pnl.gov>
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <string>
#include <vector>
#include <boost/program_options.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/format.hpp>

#include "cgnsfile.h"


/// A place to remember the name of this program
std::string program;

/// Name of discrete data node for "old" mesh info
static std::string old_cell_data_name("OriginalCell");

/// Name of  array of "old" mesh cell indexes
static std::string old_cell_index_name("OriginalCellIndex");

/// Name of discrete data node for "old" mesh info
static std::string old_vertex_data_name("OriginalVertex");

/// Name of  array of "old" mesh cell indexes
static std::string old_vertex_index_name("OriginalVertexIndex");


// -------------------------------------------------------------
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{
  boost::filesystem::path progpath(argv[0]);
  program = progpath.filename().string();

  // Parse command line options

  namespace po = boost::program_options;

  po::options_description desc("Available options");
  desc.add_options()
    ("help", "produce this help message")
    ("base", po::value<int>()->default_value(1), "CGNS base node index (1 is first)")
    ("zone", po::value<int>()->default_value(0), "zone index (0 is first)")
    // ("vertex", "get a vertex index instead of a cell index")
    ("index", po::value< std::vector<int> >(), "new cell index(es) (0 is first)")
    ("cgns", po::value< std::string >(), "input CGNS file")
    ;

  po::positional_options_description p;
  p.add("cgns", 1);
  p.add("zone", 1);
  p.add("index", -1);

  po::variables_map vm;
  po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
  po::notify(vm);    


  int base = vm["base"].as<int>();
  int zone = vm["zone"].as<int>();

  if (vm.count("help")) {
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << "\n";
    return 1;
  }

  if (vm.count("cgns") <= 0) {
    std::cerr << program << ": error: missing cgns-input" << std::endl;
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << "\n";
    return 1;
  }

  if (vm.count("index") <= 0) {
    std::cerr << program << ": error: no indexes specified -- nothing to do" << std::endl;
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << "\n";
    return 1;
  }
    
  std::string inname(vm["cgns"].as<std::string>());

  std::vector<int> newindex(vm["index"].as< std::vector<int> >());

  CGNSFile in;
  try {
    in.open(inname.c_str(), CG_MODE_READ);
    CGNSFile::BasePtr inbase = in.baseread(base);
    CGNSFile::ZonePtr inzone = in.zone(zone+1);
    
    int indesc;
    cgns::GridLocation_t dataloc;
    in.discreteread(old_cell_data_name.c_str(), indesc, dataloc);
    
    std::vector<int> oldcellidx(inzone->cells());
    in.discretefieldread(indesc, old_cell_index_name.c_str(), &oldcellidx[0]);

    for (std::vector<int>::iterator i = newindex.begin();
         i != newindex.end(); i++) {
      if (*i > inzone->cells() - 1) {
        std::cerr << boost::str(boost::format("%s: error: cell %d out of range, ignored") %
                                program % *i)
                  << std::endl;
        continue;
      }
      std::cout << boost::str(boost::format("Block %d, \"%s\", Cell %d: %d") %
                              zone % inzone->name % *i % oldcellidx[*i]) 
                << std::endl;
    }
    in.close();
  } catch (std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }
  

}

