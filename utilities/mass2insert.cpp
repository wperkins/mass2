// -------------------------------------------------------------
// file: mass2insert.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created November 17, 2017 by William A. Perkins
// Last Change: 2018-04-24 13:16:03 d3g096
// -------------------------------------------------------------

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>    // copy
#include <iterator>     // ostream_operator

#include <boost/program_options.hpp>
#include <boost/tokenizer.hpp>
#include <boost/lexical_cast.hpp>

#include "mass2solution.h"
#include "mybasename.h"

std::string program("unknown");
bool verbose(false);

typedef std::vector<std::string> svector;
typedef boost::tokenizer < boost::escaped_list_separator<char> > Tokenizer;

// -------------------------------------------------------------
// read_field_from_csv
// -------------------------------------------------------------
void
read_field_from_csv(const std::string& fname, MASS2Solution::CellFieldPtr field)
{
  std::ifstream f;

  f.open(fname.c_str());
  if (!f) {
    std::string msg(fname);
    msg += ": error: cannot open";
    throw std::runtime_error(msg);
  }

  std::string line;
  int n(0);
  std::vector<std::string> vec;
  int blk, i, j;
  double value;
  bool first(true);

  while (std::getline(f, line)) {
    Tokenizer tok(line);
    n++;
    if (!first) {
      vec.assign(tok.begin(), tok.end());
      try {
        blk = boost::lexical_cast<int>(vec[0]);
        i = boost::lexical_cast<int>(vec[1]);
        j = boost::lexical_cast<int>(vec[2]);
        value = boost::lexical_cast<double>(vec[3]);
        (*field)(i,j) = value;
      } catch (const boost::bad_lexical_cast &e) {
        std::cerr << program << ": " 
                  << fname << ", line " << n << ": error: " << e.what() 
                  << std::endl;
        continue;
      }
    }
    first = false;
  }

  if (verbose) {
    std::cerr << program << ": " 
              << fname << " successfully read"
              << std::endl;
  }
  f.close();
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
  po::variables_map vm;
  std::string inname, csvname;
  std::string field("InsertedValue");
  int thebase;
  int thezone;
  int thesidx;
  double thescale;
  std::string funits, fdesc;

  try {

    desc.add_options()
      ("help", "produce this help message")
      ("verbose", "produce some diagnostic messages")
      ("base", po::value<int>()->default_value(1), "CGNS base node index")
      ("zone", po::value<int>()->default_value(1), 
       "Index of zone whose solutions should be used (input file value is ignored)")
      ("solution-index", po::value<int>()->default_value(1),
       "Index of flow solution in which to insert or overwrite the field")
      ("field", po::value< std::string >()->default_value(field), 
       "Name of solution field(s) to insert")
      ("scale", po::value<double>()->default_value(1.0),
       "Multiply the input field by this number to scale (e.g. unit conversion)")
      ("units", po::value< std::string >(), "Units of inserted field")
      ("description", po::value< std::string >(), "Description of inserted field")
      ("cgns-input", po::value< std::string >(), "MASS2 CGNS file to change")
      ("csv-input", po::value< std::string >(), "input CSV file")
      ;
    
    po::positional_options_description p;
    p.add("cgns-input", 1);
    p.add("csv-input", 1);
    
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
    inname = vm["cgns-input"].as<std::string>();

    if (vm.count("csv-input") <= 0) {
      std::cerr << program << ": error: missing csv-input" << std::endl;
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << std::endl;
      return 1;
    }
    csvname = vm["csv-input"].as<std::string>();
    
    

    verbose = (vm.count("verbose") > 0); 
    thebase = vm["base"].as<int>();
    thezone = vm["zone"].as<int>();
    thesidx = vm["solution-index"].as<int>();
    thescale = vm["scale"].as<double>();

    if (vm.count("field") > 0) {
      field = vm["field"].as<std::string>();
      if (verbose) {
        std::cerr << program << ": info: "
                  << "inserting field with name \""
                  << field << "\"" 
                  << std::endl;
      }
    }
    
    if (vm.count("units") > 0) {
      funits = vm["units"].as<std::string>();
    }

    if (vm.count("description") > 0) {
      fdesc = vm["description"].as<std::string>();
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

  MASS2Solution mass2(inname, CG_MODE_MODIFY);
  mass2.base(thebase);
  mass2.zone(thezone);
  MASS2Solution::CellFieldPtr field_array(mass2.cell_scalar_array());

  try {
    std::string sname;
    sname = mass2.solinfo(thesidx);
    if (verbose) {
      std::cerr << program << ": " << inname << ": solution " 
                << thesidx << " is named " << sname
                << std::endl;
    }
  } catch (const CGNSFile::error& e) {
    std::cerr << program << ": " << inname << ": error: cannot find solution " 
              << thesidx 
              << ": " << e.what()
              << std::endl;
    return 3;
  }

  if (verbose) {
    std::cerr << program << ": info: reading input field \""
              << field << "\" from "
              << csvname 
              << std::endl;
  }
  read_field_from_csv(csvname, field_array);

  (*field_array) *= thescale;

  if (verbose) {
    std::cerr << program << ": " << inname 
              << ": info: field \"" << field << "\""
              << std::endl;
  }
  mass2.put_cell_field(thesidx, field, field_array);

  

  mass2.close();
  return 0;
}
