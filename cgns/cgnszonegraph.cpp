// -------------------------------------------------------------
// file: cgnszonegraph.cpp
/**
  * @file   cgnszonegraph.cpp
  * @author William A. Perkins
  * @date Thu Jun 28 10:08:01 2012
  * 
  * @brief A program to visualize (or make a visualization) of zone
  * connectivity in a CGNS file.
  * 
  * 
  */
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created July 16, 2010 by William A. Perkins
// Last Change: Thu Jun 28 10:08:01 2012 by William A. Perkins <d3g096@pe10900.pnl.gov>
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <fstream>
#include <string>
#include <set>
#include <boost/program_options.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/format.hpp>

namespace bf = boost::filesystem;

#include "cgnsfile.h"

/// A place to remember the name of this program
std::string program;

/// A global verbose mode
bool verbose(true);

/// A set o strings
typedef std::set< std::string > sset;

// -------------------------------------------------------------
// edge
// -------------------------------------------------------------
std::string
edge(const std::string& n1, const std::string& n2, const std::string& label)
{
  std::string result;

  result = 
    boost::str(boost::format("\"%s\" -- \"%s\"") % n1 % n2);
  if (!label.empty()) {
    result += 
      boost::str(boost::format("[ label = \"%s\" ]") % label);
  }
  result += ";";
  return result;
}

// -------------------------------------------------------------
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{

  // save the command name for messages

  bf::path progpath(argv[0]);
  program = progpath.filename().string();

  // Parse command line options

  namespace po = boost::program_options;

  po::options_description desc("Available options");
  po::variables_map vm;
  std::string inname;
  std::string outname;
  int inbaseidx;
  bool docyclic;
  bool doedgelabel;

  try {

    desc.add_options()
      ("help", "produce this help message")
      ("base", po::value<int>()->default_value(1), "CGNS base node index (in input)")
      ("cyclic", "show cyclic connections too (ignored by default)")
      ("label-connections", "show the names of zone connections")
      ("cgns-input", po::value< std::string >(), "input CGNS file")
      ("output", po::value< std::string >()->default_value(""), 
       "file to output DOT language graph (standard output default)")
      ;

    po::positional_options_description p;
    p.add("cgns-input", 1);
    p.add("output", 1);
    
    po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
    po::notify(vm);    
  
    if (vm.count("help")) {
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << "\n";
      return 1;
    }
    
    if (vm.count("cgns-input") <= 0) {
      std::cerr << program << ": error: missing cgns-input" << std::endl;
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << "\n";
      return 1;
    }

    inname = vm["cgns-input"].as<std::string>();
    outname = vm["output"].as<std::string>();
    inbaseidx = vm["base"].as<int>();
    docyclic = (vm.count("cyclic") > 0);
    doedgelabel = (vm.count("label-connections") > 0);

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

  // make a stream for output

  std::ofstream f;

  if (!outname.empty()) {
    f.open(outname.c_str());
    if (!f.good()) {
      std::cerr << program << ": error: cannot open " 
                << "\"" << outname << "\" for output"
                << std::endl;
      return (2);
    }
    std::cout.rdbuf(f.rdbuf());
  }


  CGNSFile icgns;

  try {
    icgns.open(inname.c_str(), CG_MODE_READ);
    icgns.base(inbaseidx);

    std::cout << "graph " << "\"" << inname << "\"" << " {"  << std::endl
              << "node [ shape=circle ]" << std::endl;

    int nzone(icgns.zones());

    sset readzones;

    for (int z = 1; z <= nzone; z++) {
      CGNSFile::ZonePtr zone(icgns.zone(z));
      int nconn(icgns.nconns());

      if (nconn <= 0) {
        std::cerr << program << ": warning: zone "
                  << zone->name << " (" << zone->index << ") "
                  << "has no connections"
                  << std::endl;
        continue;
      }
      
      for (int c = 1; c <= nconn; c++) {
        CGNSFile::ConnPtr conn(icgns.conninfo(c));
        std::string s;
        if (doedgelabel) s = conn->name;

        // if the donor zone has already been done, we can skip the connection

        if (readzones.count(conn->donorname) > 0) continue;
        if (conn->periodic && !docyclic) continue;
        std::cout << edge(conn->zone->name,conn->donorname, s) 
                  << std::endl;
      }

      readzones.insert(zone->name);

    }
    std::cout << "}" << std::endl;
    icgns.close();
  } catch (std::runtime_error& e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    return(3);
  }    
  if (f.good()) f.close();
  return 0;
}

