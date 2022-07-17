// -------------------------------------------------------------
/**
 * @file   cgnsaverage.cpp
 * @author William A. Perkins
 * @date Thu Jun 28 10:07:40 2012
 * 
 * @brief  average solutions in a transient CGNS file
 * 
 * 
 */

// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created December  9, 2009 by William A. Perkins
// Last Change: Thu Jun 28 10:07:40 2012 by William A. Perkins <d3g096@pe10900.pnl.gov>
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <iterator>
#include <vector>
#include <set>
#include <valarray>
#include <string>
#include <algorithm>

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

/// A basic integer array
typedef std::vector<int> ivector;

/// A basic double array
typedef std::vector<double> dvector;

/// A basic string vector
typedef std::vector<std::string> svector;

/// A basic string set
typedef std::set<std::string> sset;

/// A vector of CGNS files
typedef boost::ptr_vector<CGNSFile> fvector;

// -------------------------------------------------------------
// openinput
// -------------------------------------------------------------
int
openinput(const svector& namelist, const int& baseidx, fvector& files, ivector& startsol, ivector& endsol)
{
  int ierr(0);
  startsol.clear();
  startsol.reserve(namelist.size());
  endsol.clear();
  endsol.reserve(namelist.size());
  files.clear();
  files.reserve(namelist.size());

  int lastzones(0);


  for (unsigned int i = 0; i < namelist.size(); i++) {
    int start(1), end(-1), last(1);
    
    svector n;
    boost::algorithm::split(n, namelist[i], boost::algorithm::is_any_of(":"));

    // this should only happen if one of namelist is empty

    if (n.size() < 1) {
      std::cerr << program 
                << ": error: unreadable input file name: "
                << "\"" << namelist[i] << "\""
                << std::endl;
      ierr++;
    } 

    // open the file, if possible
    // check the first zone and see how many solutions there are

    int zones;
    CGNSFile* f(new CGNSFile());
    try { 
      f->open(n[0], CG_MODE_READ);
      f->base(baseidx);
      zones = f->zones();
      f->zone(1);
      last = f->nsols();
    } catch (CGNSFile::error& e) {
      ierr++;
      std::cerr << program << ": error: "
                << n[0] << ": unable to open: "
                << e.what()
                << std::endl;
      continue;
    }

    if (last <= 0) {
      ierr++;
      std::cerr << program << ": error: "
                << n[0] << ": file has no solutions"
                << std::endl;
      continue;
    }

    if (i == 0) {
      lastzones = zones;
    }
    if (zones != lastzones) {
      ierr++;
      std::cerr << program <<  ": error: "
                << n[0] << ": number of zones (" 
                << zones 
                << ") not the same as first file (" 
                << lastzones << ")"
                << std::endl;
      continue;
    }

    // get the start index if there

    if (n.size() > 1) {
      if (n[1].size() > 0) {
        try {
          start = boost::lexical_cast<int>(n[1]);
        } catch (boost::bad_lexical_cast& e) {
          ierr++;
          std::cerr << program << ": error: cannot read start index from "
                    << "\"" << namelist[i] << "\""
                    << ": " << e.what()
                    << std::endl;
          continue;
        }
      }
    }

    if (start > last) {
      std::cerr << program << ": error: start index (" << start << ") too large" << std::endl;
      ierr++;
    }
    
    if (n.size() > 2) {
      if (n[2].size() > 0) {
        try {
          end = boost::lexical_cast<int>(n[2]);
        } catch (boost::bad_lexical_cast& e) {
          ierr++;
          std::cerr << program << ": error: cannot read end index from "
                    << "\"" << namelist[i] << "\""
                    << ": " << e.what()
                    << std::endl;
          continue;
        }
      }
    }
    if (end <= 0) {
      end = last;
    }

      
    end = std::min(end, last);

    if (start > end) {
      std::cerr << program << ": error: "
                << n[0] << ": end index (" 
                << end
                << ") less than start index (" 
                << start << ")"
                << std::endl;
      ierr++;
      continue;
    }


    if (verbose) {
      std::cerr << program << ": info: "
                << n[0] << ": reading solutions " 
                << start << " to " << end 
                << " for " << zones << " zones"
                << std::endl;
    }
    files.push_back(f);
    startsol.push_back(start);
    endsol.push_back(end);

  }
  return(ierr);
}


// -------------------------------------------------------------
// generate_field_list
// -------------------------------------------------------------
int
generate_field_list(fvector& files, const svector& fldonly, 
                    const svector& fldignore, svector& flds)
{
  int ierr(0);
  
  // go through all the files, assume solution 1 in zone 1 is
  // representative, and make an available solution field list

  sset fldcommon;

  for (fvector::iterator f = files.begin(); f != files.end(); f++) {
    std::string zname(f->zone(ONE)->name);
    std::string sname(f->solinfo(ONE));
                      
    int nfld(f->nfields(ONE));

    sset fldlocal;

    for (int i = 1; i <= nfld; i++) {
      fldlocal.insert(f->fieldinfo(ONE, i));
    }

    if (fldcommon.empty()) {
      fldcommon = fldlocal;
    } else {
      sset tmp;
      std::set_intersection(fldcommon.begin(), fldcommon.end(),
                            fldlocal.begin(), fldlocal.end(),
                            std::inserter(tmp, tmp.begin()));
      fldcommon = tmp;
    }

    if (verbose) {
      std::cerr << program << ": "
                << f->name() << ": "
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
  svector inname;
  std::string outname, solname;
  int inbaseidx;
  int outbaseidx;
  svector fieldlist, onlyfields, ignorefields;

  try {

  desc.add_options()
    ("help", "produce this help message")
    ("verbose", "spew lots of information while processing")
    ("inbase", po::value<int>()->default_value(1), "CGNS base node index (in input)")
    ("outbase", po::value<int>()->default_value(1), "CGNS base node index (in output)")
    ("only", po::value< svector >(), "average only the specified field(s)")
    ("ignore", po::value< svector >(), "ignore the specified field(s)")
    ("cgns-input", po::value< svector >(), 
      
     "input CGNS file(s), format=\"name[:start[:end]]\"")
    ("cgns-output", po::value< std::string >(), 
     "existing output CGNS file (with same structure as input)")
    ("solution-name", po::value< std::string >()->default_value("Time Averaged"),
     "name of solution to produce in output")
    ;

    po::positional_options_description p;
    p.add("cgns-output", 1);
    p.add("cgns-input", -1);

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

    inname = vm["cgns-input"].as<svector>();
    outname = vm["cgns-output"].as<std::string>();
    solname = vm["solution-name"].as<std::string>();

    inbaseidx = vm["inbase"].as<int>();
    outbaseidx = vm["outbase"].as<int>();

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

  fvector infiles;
  ivector startidx;
  ivector endidx;

  if (openinput(inname, inbaseidx, infiles, startidx, endidx)) {
    std::cerr << program 
              << ": too many input errors, cannot continue"
              << std::endl;
    return(3);
  }

  if (verbose) {
    int n(infiles.size());
    int t(0);
    for (int i = 0; i < n; i++) {
      t += endidx[i] - startidx[i] + 1;
    }
    std::cerr << program << ": info: averaging "
              << t << " solutions in "
              << n << " files"
              << std::endl;
  }

  try {
    if (generate_field_list(infiles, onlyfields, ignorefields, fieldlist)) {
      std::cerr << program 
                << ": too many input errors, cannot continue"
                << std::endl;
      return(3);
    }    
  } catch (std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }


  if (verbose) {
    std::cerr << program << ": info: averaging "
              << fieldlist.size() << " fields: ";
    std::copy(fieldlist.begin(), fieldlist.end(),
              std::ostream_iterator<std::string>(std::cerr, ", "));
    std::cerr << std::endl;
  }

  CGNSFile outfile;

  try {
    int zones;

    outfile.open(outname.c_str(), CG_MODE_MODIFY);
    outfile.base(outbaseidx);
    zones = outfile.zones();

    for (int z = 1; z <= zones; z++) {
      outfile.zone(z);
      outfile.solwrite(solname.c_str());
    }
    
    for (svector::iterator flditr = fieldlist.begin(); flditr != fieldlist.end(); flditr++) {
      std::string fld(*flditr);
      if (verbose) {
        std::cerr << program << ": info: averaging field \""
                  << fld << "\""
                  << std::endl;
      }
      for (int z = 1; z <= zones; z++) {
        int cells(infiles.front().zone(z)->cells());

        outfile.zone(z);
        int outcells = outfile.zone()->cells();

        if (outcells != cells) {
          std::cerr << program << ": error: zone "
                    << z << " cell count mismatch between input and output"
                    << std::endl;
          return (3);
        }

        int outs = outfile.solindex(solname.c_str());

        dvector x(cells, 0.0);
        dvector avg(cells, 0.0);

        double nsol(0);

        for (unsigned int i = 0; i < infiles.size(); i++) {
          CGNSFile::ZonePtr zone(infiles[i].zone(z));
          for (int s = startidx[i]; s <= endidx[i]; s++) {
            std::string sname(infiles[i].solinfo(s));
            switch (infiles[i].solloc(s)) {
            case cgns::CellCenter:
              break;
            default:
              std::cerr << program << ": warning: "
                        << "skipping non-CellCenter solution \""
                        << sname << "\"" 
                        << std::endl;
              continue;
            }
            if (verbose) {
              std::cerr << program << ": info: "
                        << "accumulating field "
                        << fld << " from "
                        << infiles[i].name() << ": "
                        << zone->name << ": "
                        << infiles[i].solinfo(s)
                        << std::endl;
            }

            infiles[i].fieldread(s, fld.c_str(), &x[0]);
            for (int j = 0; j < cells; j++) avg[j] += x[j];
            nsol += 1.0;
          }
        }
        
        if (verbose) {
          std::cerr << program << ": info: "
                    << "writing averaged " << fld 
                    << " for zone " << z
                    << std::endl;
        }
        
        std::for_each(avg.begin(), avg.end(), bl::_1 /= bl::var(nsol));
        outfile.fieldwrite(outs, fld.c_str(), &avg[0]);
      }
    }

    outfile.close();
  } catch (std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }

  for (fvector::iterator f = infiles.begin(); f != infiles.end(); f++) {
    try {
      f->close();
    } catch (std::runtime_error &e) {
      std::cerr << program << ": error: " << e.what() << std::endl;
    }
  }


  return(0);
}

