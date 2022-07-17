// -------------------------------------------------------------
// file: cgnsextract.cpp
/**
 * @file   cgnsextract.cpp
 * @author William A. Perkins
 * @date Fri Dec  2 14:31:04 2011
 * 
 * @brief  A program to extract cell solution values from an unstructured zone. 
 * 
 * 
 */

// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created April  4, 2007 by William A. Perkins
// Last Change: 2014-10-14 10:13:11 d3g096
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <fstream>
#include <iomanip>
#include <string>
#include <vector>
#include <algorithm>
#include <libgen.h>
#include <boost/spirit/include/classic.hpp>
#include <boost/program_options.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/format.hpp>

#include "cgnsfile.h"

namespace bl = boost::lambda;

static std::string program;

// -------------------------------------------------------------
// read_index_list
// -------------------------------------------------------------
int
read_index_list(std::istream& infile, std::vector<int>& ilist)
{
  using namespace boost::spirit::classic;


  rule <phrase_scanner_t> index_rule = 
    int_p[push_back_a(ilist)] >>
    * ( int_p[push_back_a(ilist)] ) >>
    end_p;

  char cbuf[1024];
  int lnum(0);
  int ierr(0);

  while (infile) {
    infile.getline(cbuf, sizeof(cbuf));
    lnum++;

    parse_info<const char *> presult;

    // skip blank lines
    presult = parse(cbuf, end_p, space_p);
    if (presult.full) continue;

    presult = parse(cbuf, index_rule, space_p);
    if (!presult.full) {
      ierr++;
      std::cerr << "line " << lnum << ": parse error" << std::endl;
    }
  }
  
  return ierr;
}

// -------------------------------------------------------------
// build_index_list
// -------------------------------------------------------------
void 
build_index_list(CGNSFile& infile, std::vector<int>& ilist)
{
  int cells(infile.zone()->cells());
  ilist.resize(cells);
  int i(0);
  for_each(ilist.begin(), ilist.end(), bl::_1 = ++bl::var(i));
}
  

// -------------------------------------------------------------
// read_discrete
// -------------------------------------------------------------
/** 
 * Reads the specified variable from the specified DiscreteData node
 * in the current zone.
 * 
 * @param infile 
 * @param iddata DiscreteData
 * @param cname 
 * @param ilist 
 * @param coord 
 */
void
read_discrete(CGNSFile& in, const int& iddata, const std::string& cname, 
              const std::vector<int>& ilist, std::vector<double>& coord)
{
  int ncells = in.zone()->cells();

  if (ilist.size() != coord.size()) {
    std::string msg ("read_te2thys_coord: arrays not sized correctly");
    throw CGNSFile::error(msg);
  }

  std::string ddname;
  cgns::GridLocation_t loc;
  in.discreteread(iddata, ddname, loc);
  
  if (loc != cgns::CellCenter) {
    std::string msg = 
      boost::str(boost::format("zone \"%1%\": \"%2%\" discrete data is vertex based, cannot continue") % in.zone()->name % ddname);
    throw CGNSFile::error(msg);
  }

  std::vector<double> tmp(ncells);
  in.discretefieldread(iddata, cname.c_str(), &tmp[0]);

  for (unsigned int i = 0; i < ilist.size(); i++) {
    coord[i] = tmp[ilist[i] - 1];
  }
}
  
// -------------------------------------------------------------
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{
  program = basename(argv[0]);
  bool verbose(false);
  bool header(false);
  bool doindex(true);

                                // Parse command line options

  namespace po = boost::program_options;

  po::options_description desc("Available options");
  desc.add_options()
    ("help", "produce this help message")
    ("verbose", "print some indication of what's going on")
    ("header", "print a header w/ field names in output")
    ("noindex", "no not put the index in the output")
    ("index", po::value< std::string >(), "file with cell indexes (default=standard input)")
    ("all", "output all cells; do not use an index file")
    ("base", po::value<int>()->default_value(1), "CGNS base node index")
    ("zone", po::value<int>()->default_value(1), "zone index")
    ("discrete", po::value<int>()->default_value(1), "DiscreteData index")
    ("solution", po::value<int>()->default_value(0), "solution index (1=first, 0=last)")
    ("input", po::value< std::string >(), "input CGNS file")
    ("variable", po::value< std::vector<std::string> >(), "solution variable name")
    ("discrete-variable", po::value< std::vector<std::string> >(), "discrete data variable name")
    ("te2thys-coord", "get and output TE2THYS cell centroids")
    ;

  po::positional_options_description p;
  p.add("input", 1);
  p.add("variable", -1);

  po::variables_map vm;
  po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
  po::notify(vm);    

  if (vm.count("help")) {
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << "\n";
    return 1;
  }

  verbose = vm.count("verbose");
  header = vm.count("header");
  if (vm.count("noindex")) doindex = false;

  if (vm.count("input") <= 0) {
    std::cerr << program << ": error: missing input" << std::endl;
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << "\n";
    return 1;
  }

                                // Open and check input CGNS file
  
  std::string inname(vm["input"].as<std::string>());
  int ibase(vm["base"].as<int>());
  int izone(vm["zone"].as<int>());
  int isol(vm["solution"].as<int>());
  int idiscrete(vm["discrete"].as<int>());
  std::vector<std::string> ivars, dvars;
  bool dote2thys(vm.count("te2thys-coord") > 0);

  CGNSFile cgnsfile;

  try {
    cgnsfile.open(inname.c_str(), CG_MODE_READ);
    cgnsfile.base(ibase);
    cgnsfile.zone(izone);

    if (isol == 0) {
      isol = cgnsfile.nsols();
    }

    int nfld = cgnsfile.nfields(isol);
    for (int i = 1; i <= nfld; i++) {
      ivars.push_back(cgnsfile.fieldinfo(isol, i));
    }

    if (cgnsfile.ndiscrete() >= idiscrete) {
      int ndfld = cgnsfile.ndiscretefields(idiscrete);
      for (int i = 1; i <= ndfld; i++) {
        std::string fname;
        cgns::DataType_t ftype;
        cgnsfile.discretefieldinfo(idiscrete, i, fname, ftype);
        dvars.push_back(fname);
      }
    }

  } catch (const CGNSFile::error& e) {
    std::cerr << program << ": " << inname
              << ": error: " << e.what() << std::endl;
    exit(2);
  }
  
                                // Check and report discrete data variables
                                // to be extracted
  
  std::vector<std::string> dvarlist;
  int ierr = 0;

                                // If called for, add some variable
                                // names for TE2THYS cell coordinates.
  
  if (vm.count("discrete-variable") > 0) {
    dvarlist = vm["discrete-variable"].as< std::vector<std::string> >();
  }
  if (dote2thys) {
    dvarlist.insert(dvarlist.begin(), "CellCentroidZ");
    dvarlist.insert(dvarlist.begin(), "CellCentroidY");
    dvarlist.insert(dvarlist.begin(), "CellCentroidX");
  }
  if (!dvarlist.empty()) {
    ierr = 0;
    for (std::vector<std::string>::const_iterator i = dvarlist.begin();
         i != dvarlist.end(); i++) {
      if (std::find(dvars.begin(), dvars.end(), (*i)) == dvars.end()) {
        std::cerr << program << ": " << cgnsfile.name() 
                  << ": error: discrete variable \"" << (*i) << "\" not found"
                  << std::endl;
        ierr++;
      }
      if (ierr > 0) exit(3);
    }
  }


                                // Check and report solution variables
                                // to be extracted
  
  std::vector<std::string> varlist;

  if (vm.count("variable") <= 0) {
    varlist = ivars;
  } else {
    varlist = vm["variable"].as< std::vector<std::string> >();
    ierr = 0;
    for (std::vector<std::string>::const_iterator i = varlist.begin();
         i != varlist.end(); i++) {
      if (std::find(ivars.begin(), ivars.end(), (*i)) == ivars.end()) {
        std::cerr << program << ": " << cgnsfile.name() 
                  << ": error: solution variable \"" << (*i) << "\" not found"
                  << std::endl;
        ierr++;
      }
      if (ierr > 0) exit(3);
    }
  }

  if (verbose) {
    std::cerr << program << ": info: extracting the following fields from " 
              << inname << std::endl;
    std::cerr << "  Discrete: " << std::endl;
    for (std::vector<std::string>::const_iterator i = dvarlist.begin();
         i != dvarlist.end(); i++) {
      std::cerr << "    " << *i << std::endl;
    }
    std::cerr << "  Solution: " << std::endl;
    for (std::vector<std::string>::const_iterator i = varlist.begin();
         i != varlist.end(); i++) {
      std::cerr << "    " << *i << std::endl;
    }
  }

  
                                // Read index file or build index list
                                // FIXME: need to check for out-of-range cell id's
  std::vector<int> idlist;
  ierr = 0;

  if (vm.count("all") > 0) {
    try {
      build_index_list(cgnsfile, idlist);
    } catch (const CGNSFile::error& e) {
      std::cerr << program << ": " << inname
                << ": error: " << e.what() << std::endl;
      exit(2);
    }
  } else if (vm.count("index") > 0) {
    std::string idfilename(vm["index"].as< std::string >());
    std::ifstream ifile(idfilename.c_str());

    if (!(ifile)) {
      std::cerr << program << ": " << idfilename << ": cannot open" << std::endl;
      exit(3);
    }

    ierr = read_index_list(ifile, idlist);

    ifile.close();
  } else {
    ierr = read_index_list(std::cin, idlist);
  }

  if (ierr > 0) {
    std::cerr << program << ": error: could not read index file" << std::endl;
    exit(2);
  } else {
    if (verbose) {
      std::cerr << program << ": info: extracting fields for " 
                << idlist.size() << " indexes" << std::endl;
    }
  }

  if (idlist.empty()) {
    std::cerr << program << ": warning: index list empty, nothing to do" 
              << std::endl;
    exit(0);
  }

                                // Some place to store the data

  typedef std::vector<double> dblvect;
  std::vector<dblvect> data(dvarlist.size() + varlist.size());
  
  

                                // Read solution data an extract
                                // requested values

  for (std::vector<dblvect>::iterator i = data.begin(); i != data.end(); i++) {
    (*i).resize(idlist.size());
  }

  try {

    int n;
    switch (cgnsfile.solloc(isol)) {
    case cgns::CellCenter:
      n = cgnsfile.zone()->cells();
      break;
    case cgns::Vertex:
      n = cgnsfile.zone()->vertices();
      break;
    default:
      std::cerr << program << ": " << inname 
                << ": error: unknown solution location" 
                << std::endl;
      exit(3);
    }

    unsigned int vstart(0);

    for (unsigned int v = 0; v < dvarlist.size(); v++, vstart++) {
      read_discrete(cgnsfile, idiscrete, dvarlist[v], idlist, data[vstart]); 
    }


    for (unsigned int v = 0; v < varlist.size(); v++, vstart++) {
      std::string vname(varlist[v]);
      dblvect x(n);
      cgnsfile.fieldread(isol, vname.c_str(), &x[0]);
      int k = 0;
      for (std::vector<int>::const_iterator id = idlist.begin();
           id != idlist.end(); id++, k++) {
        int i((*id) - 1);
        data[vstart][k] = x[i];
      }
    }
    cgnsfile.close();
  } catch (const CGNSFile::error& e) {
    std::cerr << program << ": " << inname 
              << ": error: " << e.what() << std::endl;
    exit(2);
  }

  if (header) {
    std::cout << "#";
    if (doindex) {
      std::cout << " Id";
    }
    for (std::vector<std::string>::const_iterator i = dvarlist.begin();
         i != dvarlist.end(); i++) {
      std::cout << ", " << *i;
    }
    for (std::vector<std::string>::const_iterator i = varlist.begin();
         i != varlist.end(); i++) {
      std::cout << ", " << *i;
    }
    std::cout << std::endl;
  }
      

  int k = 0;
  for (std::vector<int>::const_iterator id = idlist.begin();
       id != idlist.end(); id++, k++) {
    if (doindex) {
      std::cout << std::setw(10) << (*id) << " ";
    }
    for (unsigned int v = 0; v < data.size(); v++) {
      std::cout << std::setiosflags(std::ios::scientific) << std::setprecision(8)
                << data[v][k] << " ";
    } 
    std::cout << std::endl;
  }


  return 0;

}
