// -------------------------------------------------------------
/**
 * @file   cgnstriangle.cpp
 * @author William A. Perkins
 * @date   2014-05-22 14:33:55 d3g096
 * 
 * @brief  
 * 
 * 
 */

// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created May 20, 2014 by William A. Perkins
// Last Change: 2013-05-03 12:23:12 d3g096
// -------------------------------------------------------------


#include <iostream>
#include <fstream>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>
#include <boost/format.hpp>

#include "cgnsfile.h"

/// The name of this program
std::string program;

#include <libgen.h>

// -------------------------------------------------------------
// triangular_zone
// 
// Checks to see that the current CGNS zone is unstructured and has
// triangular elements
// -------------------------------------------------------------
bool
triangular_zone(CGNSFile& file)
{
  CGNSFile::ZonePtr zone(file.zone());
  bool result(zone->type == cgns::Unstructured);

  if (result) {
    int nsect(file.nsections());
    
    for (int sect = 1; sect <= nsect; ++sect) {
      CGNSFile::SectionPtr sp(file.sectionread(sect));
      result = result && sp->type == cgns::TRI_3;
    }
  }
  return result;
}

// -------------------------------------------------------------
// write_elements
//
// Write out the element/node connectivity of the current zone in
// triangle format.
// -------------------------------------------------------------
void
write_elements(CGNSFile& file, std::ostream& out)
{
  CGNSFile::ZonePtr zone(file.zone());
  int nsect(file.nsections());

  for (int sect = 1; sect <= nsect; ++sect) {
    CGNSFile::SectionPtr sp(file.sectionread(sect));
    std::vector<cgns::cgsize_t> edata(file.elementdatasize(sp));
    file.elementsread(sp, &edata[0], NULL);
    int npe(file.npe(sp->type));
    boost::format fmt(" %10d");
    int elem(sp->idxbeg);
    for (unsigned int e = 0; e < edata.size();) {
      std::string s;
      s = boost::str(fmt % elem);
      for (int n = 0; n < npe && e < edata.size(); ++n, e++) {
        s += boost::str(fmt % edata[e]);
      }
      out << s << std::endl;
      elem++;
    }
  }
}

// -------------------------------------------------------------
// write_coordinates
//
// Write out the current zone's coordinates.  It's assumed the
// coordinates are in the correct order (x, y, z)
// -------------------------------------------------------------
void
write_coordinates(CGNSFile& file, std::ostream& out)
{
  unsigned int ncoord(file.ncoords());
  CGNSFile::ArrayPtr array(file.coordinfo(1));
  unsigned int nnodes(file.zone()->vertices());
  std::vector< std::vector<double> > c(ncoord);
  for (unsigned int i = 0; i < ncoord; ++i) {
    c[i].resize(nnodes);
    file.coordread(i+1, &(c[i][0]));
  }
  boost::format ifmt("%10d");
  boost::format cfmt(" %15.3f");
  for (unsigned int n = 0; n < nnodes; ++n) {
    std::string s(boost::str(ifmt % (n+1)));
    for (unsigned int i = 0; i < ncoord; ++i) {
      s += boost::str(cfmt % c[i][n]);
    }
    out << s << std::endl;
  }  
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
    ("cgns", po::value< std::string >(), "input CGNS file")
    ("outbase", po::value< std::string >(), "base name for output files")
    ;

  po::positional_options_description p;
  p.add("cgns", 1);
  p.add("base", 1);

  po::variables_map vm;
  po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
  po::notify(vm);    

  if (vm.count("help")) {
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << "\n";
    return 1;
  }

  int base = vm["base"].as<int>();
  int zone = vm["zone"].as<int>();
  if (vm.count("cgns") <= 0) {
    std::cerr << program << ": error: CGNS file name missing" << std::endl;
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << "\n";
    return 1;
  }
  std::string cgnsname(vm["cgns"].as<std::string>());

  std::string elementname;
  std::string nodename;
  {
    boost::filesystem::path p;
    if (vm.count("outbase") > 0) {
      p = boost::filesystem::path(vm["outbase"].as<std::string>());
    } else {
      p = boost::filesystem::path(cgnsname);
    }
    p.replace_extension(".elem");
    elementname = p.string();
    p.replace_extension(".node");
    nodename = p.string();
  }

                                // Check the CGNS file 

  CGNSFile cgns;
  try {
    cgns.open(cgnsname.c_str(), CG_MODE_READ);
    cgns.base(base);
    cgns.zoneread(zone);

    if (!triangular_zone(cgns)) {
      std::cerr << argv[0] << ": error: " << cgnsname 
                << ": triangular unstructured zones only only" << std::endl;
      return 3;
    }

    std::ofstream out;
    out.open(elementname.c_str());
    write_elements(cgns, out);
    out.close();

    out.open(nodename.c_str());
    write_coordinates(cgns, out);
    out.close();

    cgns.close();

  } catch (std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }

  return 0;
}
