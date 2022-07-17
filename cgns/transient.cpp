// -------------------------------------------------------------
/**
 * @file   transient.cpp
 * @author William A. Perkins
 * @date Thu Jun 28 10:32:27 2012
 * 
 * @brief A program to build the necessary nodes to make a CGNS file
 * with a series of solutions into a TimeAccurate CGNS.  The input
 * file is modified directly. A constant number of zones is assumed.
 * It is also assumed that all zones have the same number of solutions
 * as the selected zone.
 * 
 */
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created January  6, 2005 by William A. Perkins
// Last Change: Thu Jun 28 10:32:27 2012 by William A. Perkins <d3g096@pe10900.pnl.gov>
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <boost/program_options.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/format.hpp>
#include <boost/algorithm/string.hpp>


#include "cgnsfile.h"

/// A place to remember the name of this program
std::string program;

// -------------------------------------------------------------
// basename
// -------------------------------------------------------------
const std::string
mybasename(const char* p)
{
  boost::filesystem::path progpath(p);
  std::string bname(progpath.filename().string());
  return bname;
}

// -------------------------------------------------------------
// te2thys_time
// -------------------------------------------------------------
double
te2thys_time(const std::string& sname)
{
  std::string t1(sname);

  while (boost::contains(t1, "=")) {
    boost::trim_left_if(t1, !boost::is_any_of("="));
    boost::trim_left_if(t1, boost::is_any_of("= "));
  } 

  double time;
  time = boost::lexical_cast<double>(t1);

  return time;
}
  
  

// -------------------------------------------------------------
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{
  program = mybasename(argv[0]);

  // Parse command line options

  namespace po = boost::program_options;

  po::options_description desc("Available options");
  desc.add_options()
    ("help", "produce this help message")
    ("verbose", "produce some diagnostic messages")
    ("force", "overwrite any existing transient information")
    ("base", po::value<int>()->default_value(1), "CGNS base node index")
    ("zone", po::value<int>()->default_value(1), "Index of zone whose solutions should be used")
    ("start", po::value<double>()->default_value(0.0), "Time of first solution")
    ("step", po::value<double>()->default_value(0.0), "Time between solutions")
    ("te2thys", "Assume input was produced by TE2THYS and extract times from solution names")
    ("cgns-input", po::value< std::string >(), "input CGNS file")
    ;

  po::positional_options_description p;
  p.add("cgns-input", 1);

  po::variables_map vm;
  po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
  po::notify(vm);    


  if (vm.count("help")) {
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << std::endl;
    return 1;
  }

  if (vm.count("cgns-input") <= 0) {
    std::cerr << program << ": error: missing cgns-input" << std::endl;
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << std::endl;
    return 1;
  }
  
  std::string inname(vm["cgns-input"].as<std::string>());
  int base = vm["base"].as<int>();
  int zone = vm["zone"].as<int>();
  double start = vm["start"].as<double>();
  double step = vm["step"].as<double>();
  bool verbose = (vm.count("verbose") > 0); 
  bool dote2thys = (vm.count("te2thys") > 0);
  bool doforce = (vm.count("force") > 0);

  CGNSFile incgns;

  std::vector<double> times;
  std::vector<int> numzones;
  std::vector<std::string> znames;
  std::vector<char> znamebuf;

  try {

    if (verbose) 
      std::cerr << boost::str(boost::format("%s: info: opening input file \"%s\"") %
                              program % inname) << std::endl;
    incgns.open(inname.c_str(), CG_MODE_MODIFY);

    CGNSFile::BasePtr inbase = incgns.baseread(base);
    if (verbose) 
      std::cerr << boost::str(boost::format("%s: %s: opening base node %d, \"%s\"") %
                              program % inname % base % inbase->name) << std::endl;

    // check to see if there is some iterative data there already

    try {
      std::string name;
      int nsteps;
      incgns.biterread(name, nsteps);
      if (doforce) {
        std::cerr <<  boost::str(boost::format("%s: warning: overwriting existing iterative data \"%s\"") %
                                 program %  name) << std::endl;
      } else {
        std::cerr <<  boost::str(boost::format("%s: error: iterative data \"%s\" already exists with %d steps, use --force to overwrite") %
                                 program % name % nsteps) << std::endl;
        incgns.close();
        return(3);
      }
    } catch (const CGNSFile::error &e) {
      if (verbose) {
        std::cerr << e.what() << std::endl;
        std::cerr <<  boost::str(boost::format("%s: %s: no iterative data present -- good") %
                                 program % inname ) << std::endl;
      }
    }


    // collect zone names

    int nz = incgns.zones();
    znames.resize(nz);
    for (int i = 0; i < nz; i++) {
      CGNSFile::ZonePtr z = incgns.zone(i+1);
      znames[i] = z->name;
    }

    if (verbose) 
      std::cerr << boost::str(boost::format("%s: %s: collected names for %d zones")  %
                              program % inname % nz) << std::endl;

    // select the example zone from which to extract solution info

    incgns.zone(zone);
    int nsols = incgns.nsols();

    if (verbose) 
      std::cerr << boost::str(boost::format("%s: %s: found %d solutions in zone %d")  %
                              program % inname % nsols % zone) << std::endl;
    numzones.resize(nsols);
    std::fill(numzones.begin(), numzones.end(), nz);

    // make a list of solution times

    times.resize(nsols);
    for (int i = 0; i < nsols; i++) {
      if (dote2thys) {
        std::string sname(incgns.solinfo(i+1));
        try {
          times[i] = te2thys_time(sname);
          if (verbose) 
            std::cerr << boost::str(boost::format("%s: info: got time %.8g TE2THYS solution \"%s\"") %
                                    program % times[i] % sname) 
                      <<  std::endl;
        } catch (const boost::bad_lexical_cast &e) {
          std::cerr << boost::str(boost::format("%s: %s: warning: solution %d, \"%s\", not a TE2THYS solution, skipped it") %
                                  program % inname % (i+1) % sname)
                    << std::endl;
        }
      } else {
        times[i] = start + i*step;
      }
    }

    // make a list of zone names

    znamebuf.resize(32*nz*nsols, ' ');
    for (int itime = 0; itime < nsols; itime++) {
      for (int i = 0; i < nz; i++) {
        int offset = 32*(itime*nz + i);
        znames[i].copy(&znamebuf[offset], 32);
      }
    }
    
    incgns.simulationtypewrite(cgns::TimeAccurate);

    if (verbose) 
      std::cerr << boost::str(boost::format("%s: %s: writing base iterative data")  %
                              program % inname) << std::endl;
    incgns.biterwrite(nsols, "TransientBase",
                      &times[0], &numzones[0], &znamebuf[0]);

    incgns.close();

  } catch (const CGNSFile::error &e) {
    std::cerr << program << ": error: " << e.what()<< std::endl;
    exit(3);
  }
  
  return (0);
}

