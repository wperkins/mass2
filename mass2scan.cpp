// -------------------------------------------------------------
// file: mass2scan.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created April  6, 2018 by William A. Perkins
// Last Change: 2019-02-21 13:08:47 d3g096
// -------------------------------------------------------------


#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <boost/utility.hpp>
#include <boost/program_options.hpp>
#include <boost/format.hpp>

#include "mass2series.h"
#include "mybasename.h"
#include "DateTime.h"

std::string program("unknown");

typedef std::vector<std::string> svector;

// -------------------------------------------------------------
// Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{
  program = mybasename(argv[0]);

  // Parse command line options

  namespace po = boost::program_options;

  po::options_description desc("Available options");
  po::variables_map vm;
  svector inname;
  DateTime date0(boost::posix_time::not_a_date_time);
  std::string outname;
  bool verbose;
  bool doskip(true);
  bool dotime(true);
  bool noneg(false);

  try {

    desc.add_options()
      ("help", "produce this help message")
      ("verbose", "produce some diagnostic messages")
      ("no-skip", "do not try to skip time overlap -- put all time slices in output")
      ("no-time", "do not put the elapsed time, s, in first output field")
      ("zero", po::value<DateTime>(&date0),
       "date/time (YYYY-Mon-dd HH:MM:SS) at which time is 0.0")
      ("no-negative-time", "Do not include times that are less than --zero") 
      ("cgns-input", po::value< svector >(), "input MASS2 CGNS file")
      ("output", po::value<std::string>(), "Write output to named file")
      ;
    
    po::positional_options_description p;
    p.add("cgns-input", -1);
    
    po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
    po::notify(vm);    
    
    
    if (vm.count("help")) {
      std::cerr << program << ": scan MASS2 CGNS files and produce a PT6 input file" << std::endl;
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
    inname = vm["cgns-input"].as<svector>();

    verbose = (vm.count("verbose") > 0); 

    outname.clear();
    if (vm.count("output") > 0) {
      outname = vm["output"].as<std::string>();
    }

    if (vm.count("no-skip") > 0) {
      doskip = false;
    }

    if (vm.count("no-time") > 0) {
      dotime = false;
    }

    if (vm.count("zero") > 0) {
      date0 = vm["zero"].as<DateTime>();
    }

    noneg = (vm.count("no-negative-time") > 0); 
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
  std::streambuf *coutbuf = std::cout.rdbuf();

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

  // Go through the series 

  try {
    boost::scoped_ptr<MASS2Series> input(new MASS2Series(inname));
    bool first(true);
    std::string lastfile;
    DateTime lastdate(boost::posix_time::neg_infin);

    while (input->advance()) {

      DateTime thedate(strp_datetime(input->current_sname().c_str(), mass2_datetime_fmt));
      if (first && date0.is_not_a_date_time()) {
        date0 = thedate;
      }
      Duration dt(thedate - date0);
      double dts(dt.total_seconds());
      const std::string outfmt("%s %5d # %s");
      std::string fname(input->current_name());
      int sidx(input->current_sindex());
      std::string sname(input->current_sname());

      bool doit(thedate > lastdate);
      if (noneg) {
        doit = doit && (thedate >= date0);
      }

      if (doit) {
        if (dotime) {
          std::cout << boost::str(boost::format("%12.1f ") % dts);
        } 
        std::cout << boost::str(boost::format(outfmt) % fname % sidx % sname);
        std::cout << std::endl;
        if (verbose) {
          if (fname != lastfile) {
            std::cerr << "Scanning " << fname 
                      << " starting with " << sidx 
                      << ": " << sname
                      << std::endl;
          }
        }
        lastdate = thedate;
      } else {
        if (verbose) {
          std::cerr << "Skipping " << fname
                    << ", " << sidx << ": " << sname 
                    << std::endl;
        }
      }

      if (!doskip) lastdate = thedate;

      first = false;
      lastfile = fname;
    }

  } catch (const CGNSFile::error &e) {
    std::cerr << program << ": error: " << e.what()<< std::endl;
    exit(3);
  }

  if (f.good()) {
    std::cout.rdbuf(coutbuf);
    f.close();
  }
  
  return 0;
}
