// -------------------------------------------------------------
// file: cgnscopyfield.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created November 30, 2009 by William A. Perkins
// Last Change: Thu Jun 28 10:07:49 2012 by William A. Perkins <d3g096@pe10900.pnl.gov>
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <iterator>
#include <vector>
#include <set>
#include <valarray>
#include <string>

#include <boost/program_options.hpp>

#include <boost/filesystem/operations.hpp>
#include <boost/lambda/lambda.hpp>

#include <boost/algorithm/string.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/lexical_cast.hpp>

namespace bf = boost::filesystem;
namespace bl = boost::lambda;

#include "cgnsfile.h"

/// Unity constant
static const int ONE(1);

/// A place to remember the name of this program
std::string program;

/// A global verbose mode
bool verbose(true);

/// A basic double array
typedef std::vector<double> dvector;

/// A basic string vector
typedef std::vector<std::string> svector;

/// A basic string set
typedef std::set<std::string> sset;

/// A basic string vector
typedef std::vector<std::string> svector;

// -------------------------------------------------------------
// generate_field_list
// -------------------------------------------------------------
int
generate_field_list(CGNSFile& f, const int& sidx, 
                    const svector& fldonly, const svector& fldignore, 
                    svector& flds)
{
  int ierr(0);
  
  sset fldcommon;

  std::string zname(f.zone(ONE)->name);
  std::string sname(f.solinfo(ONE));
  
  int nfld(f.nfields(ONE));

  sset fldlocal;

  for (int i = 1; i <= nfld; i++) {
    fldlocal.insert(f.fieldinfo(ONE, i));
  }

  if (verbose) {
    std::cerr << program << ": "
              << f.name() << ": "
              << zname << ": " 
              << sname << ": fields: ";
    std::copy(fldlocal.begin(), fldlocal.end(), 
              std::ostream_iterator<std::string>(std::cerr, ", "));
    std::cerr << std::endl;
    std::cerr << program << ": common fields: ";
    std::copy(fldcommon.begin(), fldcommon.end(), 
              std::ostream_iterator<std::string>(std::cerr, ", "));
    std::cerr << std::endl;
  }
  
  // If some specific fields have been specified, check the name against those available

  if (!fldonly.empty()) {
    for (svector::const_iterator fld = fldonly.begin(); fld != fldonly.end(); fld++) {
      if (fldcommon.find(*fld) == fldcommon.end()) {
        std::cerr << program << ": error: "
                  << "specified field \"" << *fld << "\" not available in all files"
                  << std::endl;
        ierr++;
      }
    }
    flds = fldonly;
  } else {
    if (!fldignore.empty()) {
      for (svector::const_iterator fld = fldignore.begin(); fld != fldignore.end(); fld++) {
        if (fldcommon.erase(*fld) < 1) {
          std::cerr << program << ": warning: "
                    << "ignored field \"" << *fld << "\" not available in all files"
                    << std::endl;
        }
      }
    }
    flds.clear();
    flds.reserve(fldcommon.size());
    std::copy(fldcommon.begin(), fldcommon.end(),
              std::back_inserter(flds));
  }
  return ierr;
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
  std::string outname, solname;
  int inbaseidx;
  int outbaseidx;
  int insolidx;
  int outsolidx;
  svector fieldlist, onlyfields, ignorefields;

  try {

    desc.add_options()
      ("help", "produce this help message")
      ("verbose", "spew lots of information while processing")
      ("input-base", po::value<int>()->default_value(1), "CGNS base node index (in input)")
      ("output-base", po::value<int>()->default_value(1), "CGNS base node index (in output)")
      ("only", po::value< svector >(), "copy only the specified field(s)")
      ("ignore", po::value< svector >(), "do not copy the specified field(s)")
      ("input-solution",  po::value<int>()->default_value(-1), 
       "solution (index) from which to copy the field(s) (<1 means last)")
      ("output-solution",  po::value<int>()->default_value(-1), 
       "solution (index) from which to copy the field(s) (<1 means last)")
      ("input-cgns", po::value< std::string >(), "input CGNS file")
      ("output-cgns", po::value< std::string >(), "existing output CGNS file (with same structure as input)")
      ;
    
    po::positional_options_description p;
    p.add("cgns-output", 1);
    p.add("cgns-input",  1);
    p.add("only", -1);

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

    if (vm.count("cgns-output") <= 0) {
      std::cerr << program << ": error: missing cgns-output" << std::endl;
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << "\n";
      return 1;
    }

    inname = vm["input-cgns"].as<std::string>();
    outname = vm["output-cgns"].as<std::string>();

    inbaseidx = vm["input-base"].as<int>();
    outbaseidx = vm["output-base"].as<int>();

    insolidx = vm["input-solution"].as<int>();
    outsolidx = vm["output-solution"].as<int>();

    if (vm.count("only")) {
      onlyfields = vm["only"].as<svector>();
    } 

    if (vm.count("ignore")) {
      ignorefields = vm["ignore"].as<svector>();
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

  
  CGNSFile infile;
  CGNSFile outfile;

  try {
    infile.open(inname.c_str(), CG_MODE_READ);
    infile.base(inbaseidx);

    int nzones = infile.zones();

    outfile.open(outname.c_str(), CG_MODE_MODIFY);
    outfile.base(outbaseidx);

    if (outfile.zones() != nzones) {
      std::cerr << program << ": error: input and output zone mismatch (" 
                << nzones << " != " << outfile.zones() << ")"
                << std::endl;
      return 2;
    }

    if (insolidx < 1)  {
      insolidx = infile.nsols();
    } else if (insolidx > infile.nsols()) {
      std::cerr << program << ": error: input solution index out of range (" 
                << insolidx << " > " << infile.nsols() << ")"
                << std::endl;
      return 2;
    }

    if (outsolidx < 1) {
      outsolidx = outfile.nsols();
    } else if (outsolidx > outfile.nsols()) {
      std::cerr << program << ": error: output solution index out of range (" 
                << outsolidx << " > " << outfile.nsols() << ")"
                << std::endl;
      return 2;
    }

    if (generate_field_list(infile, insolidx, onlyfields, ignorefields, fieldlist)) {
      std::cerr << program 
                << ": too many input errors, cannot continue"
                << std::endl;
      return(3);
    }      

    for (int z = 1; z < nzones; z++) {
      std::string insname(infile.solinfo(insolidx));
      std::string outsname(outfile.solinfo(outsolidx));

      int incells(infile.zone()->cells());
      int outcells(outfile.zone()->cells());
      
      if (incells != outcells) {
        std::cerr << program << ": error: cell count mismatch, zone " 
                  << z << " (" << incells << " != " << outcells << ")"
                  << std::endl;
        return 2;
      }

      int sloc(infile.solloc(insolidx));
      
      if (sloc != outfile.solloc(outsolidx)) {
        std::cerr << program << ": error: solution location mismatch, zone " 
                  << z << " (" << sloc << " != " << outfile.solloc(outsolidx) << ")"
                  << std::endl;
        return 2;
      }

      int ndata;
      switch (sloc) {
      case cgns::CellCenter:
        ndata = incells;
        break;
      default:
        std::cerr << program << ": error: unsupported solution location mismatch, zone " 
                  << z << " (" << sloc << ")"
                  << std::endl;
        return 2;
      }

      dvector x(ndata);

      for (svector::iterator fld = fieldlist.begin(); fld != fieldlist.end(); fld++) {
        if (verbose) {
          std::cerr << program << ": info: copying field \"" << *fld << "\""
                    << " from \"" << insname << "\" to \"" << outsname << "\"" 
                    << std::endl;
        }

        infile.fieldread(insolidx, fld->c_str(), &x[0]);
        outfile.fieldwrite(outsolidx, fld->c_str(), &x[0]);
      }

    }

    infile.close();
    outfile.close();
  } catch (std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    return 2;
  }


  return 0;
}
