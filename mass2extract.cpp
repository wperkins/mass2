// -------------------------------------------------------------
// file: mass2extract.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created November 20, 2018 by William A. Perkins
// Last Change: 2018-12-03 10:07:12 d3g096
// -------------------------------------------------------------

#include <iostream>
#include <fstream>
#include <vector>
#include <boost/program_options.hpp>
#include <boost/format.hpp>
#include <boost/scoped_ptr.hpp>
#include "boost/date_time/posix_time/posix_time.hpp"

#include "mass2series.h"
#include "mybasename.h"

std::string program("unknown");

typedef boost::gregorian::date Date;
typedef boost::posix_time::ptime DateTime;
typedef boost::posix_time::time_duration Duration;
typedef std::vector<std::string> svector;

std::string thesep(", ");

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
  po::variables_map vm;
  svector inname;
  std::string listname;
  std::string outname;
  bool verbose;
  int thebase;
  int thezone;
  int i_index;
  int j_index;

  svector field;
  bool dowet(false);
  bool doskip(true);
  bool dometric(false);
  bool header(true);
  bool doall;

  double xmin, xmax, ymin, ymax;

  try {

    desc.add_options()
      ("help", "produce this help message")
      ("verbose", "produce some diagnostic messages")
      ("base", po::value<int>()->default_value(1), "CGNS base node index")
      ("zone", po::value<int>()->default_value(1), 
       "Index of zone whose solutions should be used")
      ("no-skip", "do not skip adjacent, indentically named solutions")
      ("field", po::value< svector >(), "Solution field(s) of interest")
      ("all", "Extract all fields from the specified solution")
      // ("metric", "Convert output values to SI units")
      ("i", po::value<int>()->default_value(-1), "The longitudinal cell index (0-based)")
      ("j", po::value<int>()->default_value(-1), "The lateral cell index (0-based)")
      ("solution-list",  po::value<std::string>()->default_value(""),
       "Read file/solution list from named file")
      ("cgns-input", po::value< svector >(), "input CGNS file(s)")
      ("output", po::value<std::string>(),
       "Write statistics output to specified file")
      ;
    
    po::positional_options_description p;
    p.add("cgns-input", -1);
    
    po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
    po::notify(vm);    
    
    
    if (vm.count("help")) {
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << std::endl;
      return 1;
    }

    i_index = vm["i"].as<int>();
    j_index = vm["j"].as<int>();

    if (i_index < 0 || j_index < 0) {
      std::cerr << program << ": error: --i and --j must be greater than 0"
                << std::endl;
      return 1;
    }

    // if a solution list file is specified, ignore any files on the
    // command line.

    listname = vm["solution-list"].as<std::string>();
    if (listname.empty()) {
      if (vm.count("cgns-input") <= 0) {
        std::cerr << program << ": error: missing cgns-input" << std::endl;
        std::cerr << "Usage: " << program << " [options]" << std::endl;
        std::cerr << desc << std::endl;
        return 1;
      }
      inname = vm["cgns-input"].as<svector>();
    }

    verbose = (vm.count("verbose") > 0); 
    thebase = vm["base"].as<int>();
    thezone = vm["zone"].as<int>();

    doall = (vm.count("all") > 0);

    if (!doall) {
      if (vm.count("field") > 0) {
        field = vm["field"].as<svector>();
        if (verbose) {
          std::cerr << program << ": info: "
                    << "computing stats for ";
          std::copy(field.begin(), field.end(),
                    std::ostream_iterator<std::string>(std::cerr, ", "));
          std::cerr << std::endl;
        }
      }
    }

    if (vm.count("no-skip") > 0) doskip = false;

    // if (vm.count("metric") > 0) dometric = true;


    if (vm.count("output") > 0) {
      outname = vm["output"].as<std::string>();
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


  
  try {
    

    boost::scoped_ptr<MASS2Series> input;
    if (!listname.empty()) {
      input.reset(new MASS2Series(listname));
    } else {
      input.reset(new MASS2Series(inname));
    }

    bool first(true);
    std::string lastfile("bogus");
    std::string lastsoln("bogus");

    while (input->advance()) {

      if (lastfile != input->current_name()) {
        lastfile = input->current_name();
        if (verbose) {
          std::cerr << program << ": info: processing CGNS file " 
                    << "\"" << lastfile << "\""
                    << std::endl;
        }
      }

      if (first) {

        // get all field names, if called for
        if (doall) {
          field.clear();
          const MASS2Solution *s(input->current());
          int sidx(input->current_sindex());
          int nfld = s->nfields(sidx);
          for (int f = 1; f <= nfld; ++f) {
            field.push_back(s->fieldinfo(sidx, f));
          }
        }

        // write a header to output, if called for
        if (header) {
          std::cout << "Date-Time" << thesep
                    << "i" << thesep
                    << "j";
          for (svector::iterator f = field.begin(); f != field.end(); ++f) {
            std::cout << thesep << *f;
          }
          std::cout << std::endl;
        }
      }

      int isol(input->current_sindex());
      std::string sname(input->current_sname());
      if (doskip) {
        if (sname == lastsoln) {
          if (verbose) {
            std::cerr << program << ": info: " << lastfile << ": skipping duplicate solution " 
                      << isol << ", "<< "\"" << sname << "\""
                      << std::endl;
          }
          continue;
        }
      }
      lastsoln = sname;
              
      if (verbose) {
        std::cerr << program << ": info: " << lastfile << ": processing solution " 
                  << isol << ", "<< "\"" << sname << "\""
                  << std::endl;
      }

      std::cout << sname << thesep
                << i_index << thesep
                << j_index;

      for (svector::iterator f = field.begin(); f != field.end(); ++f) {
        MASS2Solution::CellFieldPtr fielddata(input->get_cell_field(*f));

        // CGNS likes column ordered arrays (like stupid Fortran)
        
        double value((*fielddata)(j_index, i_index));

        std::cout << boost::str(boost::format("%c%15.10g") % thesep % value);
      }
      
      std::cout << std::endl;
      
      first = false;
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
