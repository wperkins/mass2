// -------------------------------------------------------------
// file: mass2dewater.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created February 19, 2019 by William A. Perkins
// Last Change: 2019-02-21 11:48:15 d3g096
// -------------------------------------------------------------


#include <iostream>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <vector>
#include <boost/program_options.hpp>
#include <boost/spirit/include/classic.hpp>
#include <boost/format.hpp>

#include "mybasename.h"
#include "mass2series.h"
#include "DateTime.h"

std::string program("unknown");

typedef std::vector<std::string> svector;

static const double sqft2ha = 9.290304e-06;

// -------------------------------------------------------------
// flood
// -------------------------------------------------------------
void
flood(const int& i, const int& j,
      const MASS2Solution::CellField& isdry, MASS2Solution::CellField& river)
{
  MASS2Solution::Shape theshape(isdry.shape());
  if (j < 0 || j > theshape[0] - 1 or i < 0 || i > theshape[1] - 1) {
    return;
  }
  if ((isdry(j, i) < 0.5) && (river(j, i) < 0.5)) {
    river(j, i) = 1;
    flood(i + 1, j    , isdry, river);
    flood(i - 1, j    , isdry, river);
    flood(i    , j + 1, isdry, river);
    flood(i    , j - 1, isdry, river);
  }
}

// -------------------------------------------------------------
// read_quadrant
//
// This reads a CSV file that assigns a "segment" and "quadrant" to
// each block i, j in the MASS2 domain (or just part of the domain, if
// desired).  Here's a sample
//
//   block,i,j,segment,quadrant
//   [...]
//  1,779,10,2,68
//  1,779,11,2,68
//  1,779,12,2,68
//  1,779,13,2,68
//  1,779,14,2,67
//  1,779,15,2,68
//  1,779,16,2,67
//  1,779,17,2,68
//  1,779,18,2,67
//  1,779,19,2,67
//  1,779,20,2,67
//  1,779,21,2,67
//  1,779,22,2,67
//  1,779,23,2,67
//  1,779,24,2,67
//  [...]
//
// Only the "quadrant" field is used. By default, all cells are
// assigned a "quadrant' of 0.  An entry for cell (i,j) in this file
// will change that.
// -------------------------------------------------------------
void
read_quadrant(const std::string& name, blitz::Array<int, 2>& quadrant)
{
  using namespace boost::spirit;
  using namespace boost::spirit::classic;
  
  std::ifstream f;

  f.open(name.c_str());
  if (!f) {
    std::string msg(name);
    msg += ": error: cannot open";
    throw std::runtime_error(msg);
  }

  int blk, i, j, sidx, qidx;

  chlit<> dq = ch_p('"');
  chlit<> sq = ch_p('\'');
  chlit<> comma = ch_p(',');

  // this should match a comment or a blank line
  rule <phrase_scanner_t> comment = ( ( '#' >> *(anychar_p) >> end_p)  || end_p );
    
  // this rule defines the record format
  rule <phrase_scanner_t> fldrule =

                                // MASS2 block index

    int_p[assign_a(blk)] >> comma >>

                                // MASS2 cell indexes i and j

    int_p[assign_a(i)] >> comma >>
    int_p[assign_a(j)] >> comma >>

                                // Segment index (not used), assumed zero if blank

    (int_p[assign_a(sidx)] || *(space_p)) >> comma >>

                                // Quadrant index, assumed zero if blank
    
    (int_p[assign_a(qidx)] || *(space_p)) >> 
  
                                // there may be a comment or just the end
    
    ( ( '#' >> *anychar_p >> end_p)  || end_p );

  int ierr = 0;
  int lnum = 0;
  char cbuf[1024];

  quadrant = 0.0;

  f.getline(cbuf, sizeof(cbuf)); // skip header line

  while (f) {

    f.getline(cbuf, sizeof(cbuf));
    lnum++;

    sidx = 0;
    qidx = 0;

    parse_info<const char *> presult;

    // check for comments first, if it matches go to the next line
    presult = parse(cbuf, comment, space_p);
    if (presult.full) continue;

    // this should be a non-empty line containing all necessary info,
    // so put it in the list
    presult = parse(cbuf, fldrule, space_p);

    if (!presult.hit) {
      std::cerr << name << ": error, line " << lnum << ": cannot parse" << std::endl;
      ierr++;
      continue;
    } else {

      quadrant(j, i) = qidx;

      if (!presult.full) {
        std::cerr << name << ": warning, line " << lnum 
                  << ": ignoring from \"" 
                  << presult.stop << "\"" << std::endl;
      }
    }
  }

  f.close();

  if (ierr) {
    std::string msg(name);
    msg += ": error: too many errors";
    throw std::runtime_error(msg);
  }
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
  svector inname;
  std::string auxname("grid.cgns");
  std::string quadname;
  std::string listname;
  std::string outname;
  bool verbose;
  int thebase;
  int thezone;
  int iriver(-1), jriver(-1);
  DateTime dstart(boost::posix_time::neg_infin);
  DateTime dend(boost::posix_time::pos_infin);
  
  try {

    desc.add_options()
      ("help", "produce this help message")
      ("verbose", "produce some diagnostic messages")
      ("base", po::value<int>()->default_value(1), "CGNS base node index")
      ("zone", po::value<int>()->default_value(1), 
       "Index of zone whose solutions should be used")
      ("i-river", po::value<int>()->default_value(iriver),
       "Longitudinal index of a point known to be always in the river")
      ("j-river", po::value<int>()->default_value(iriver),
       "Lateral index of a point known to be always in the river")
      ("begin", po::value<DateTime>(&dstart),
       "Date/time (format: YYYY-Mon-dd HH:MM:SS) to begin extraction")
      ("end", po::value<DateTime>(&dend),
       "Date/time (format: YYYY-Mon-dd HH:MM:SS) to end extraction")
      ("auxiliary", po::value<std::string>()->default_value(auxname),
       "Name of auxiliary CGNS file that matches --cgns-input")
      ("quadrant", po::value<std::string>()->default_value(""),
       "Name of CSV file containing quadrant indexes for each (i, j)")
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

    // if a solution list file is specified, ignore any files on the
    // command line.

    auxname = vm["auxiliary"].as<std::string>();
    listname = vm["solution-list"].as<std::string>();
    if (listname.empty()) {
      if (vm.count("cgns-input") <= 0) {
        throw(po::error("missing --cgns-input"));
      }
      inname = vm["cgns-input"].as<svector>();
    }

    verbose = (vm.count("verbose") > 0); 
    thebase = vm["base"].as<int>();
    thezone = vm["zone"].as<int>();

    iriver = vm["i-river"].as<int>();
    jriver = vm["j-river"].as<int>();
    if (iriver < 0 || jriver < 0) {
      throw(po::error("a known wet location must be specified with --i-river and --j-river"));
    }

    quadname = vm["quadrant"].as<std::string>();

    if (vm.count("begin")) {
      dstart = vm["begin"].as<DateTime>();
    }
    if (vm.count("end")) {
      dend = vm["end"].as<DateTime>();
    }

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

      
    MASS2Solution::CellFieldPtr cell_area;
    {
      svector tmp;
      tmp.push_back(auxname);
      boost::scoped_ptr<MASS2Series> aux(new MASS2Series(tmp));

      while (aux->advance()) {
        if (aux->current_sname() == "GridMetrics") {
          cell_area = aux->get_cell_field("hp1");
          MASS2Solution::CellFieldPtr tmp;
          tmp = aux->get_cell_field("hp2");
          *(cell_area) *= *(tmp);
          break;
        }
      }
    }

    // std::cout << "The Shape: " << cell_area->shape() << std::endl;

    if (!cell_area) {
      throw(std::runtime_error("Auxiliary file does not contain GridMetrics"));
    }
    
    boost::scoped_ptr<MASS2Series> input;
    if (!listname.empty()) {
      input.reset(new MASS2Series(listname));
    } else {
      input.reset(new MASS2Series(inname));
    }

    input->base(thebase);
    input->zone(thezone);

    std::string lastfile("bogus");
    std::string lastsoln("bogus");

    MASS2Solution::CellField river(cell_area->shape());
    MASS2Solution::CellField lastriver(cell_area->shape());
    MASS2Solution::CellField river_area(cell_area->shape());
    MASS2Solution::CellField outriver(cell_area->shape());
    MASS2Solution::CellField strand(cell_area->shape());
    MASS2Solution::CellField entrap(cell_area->shape());


    int qmin(0), qmax(0);
    blitz::Array<int, 2> quad(cell_area->shape());
    std::vector<double> qarea;
    if (!quadname.empty()) {
      read_quadrant(quadname, quad);
      // assuming quadrant indexes are nonnegative
      qmin = static_cast<int>(blitz::min(quad));
      qmax = static_cast<int>(blitz::max(quad));
      qarea.resize(qmax, 0.0);
      for (int q = qmin; q <= qmax; ++q) {
        double qcell(blitz::sum(blitz::where(quad == q, *(cell_area), 0.0)));
        qarea[q] = qcell;
      }
    } else {
      quad = 0.0;
      qarea.resize(1, 0.0);
    }

    bool first(true);
      
    while (input->advance()) {
      std::string sname(input->current_sname());
      DateTime thedate(strp_datetime(sname.c_str(), "%m-%d-%Y %H:%M:%S"));

      if (thedate < dstart) continue;
      if (thedate > dend) break;

      MASS2Solution::CellFieldPtr isdry(input->get_cell_field("isdry"));
      river = 0.0;
      flood(iriver, jriver, *isdry, river);
      if (first) lastriver = river;
      river_area = *(cell_area) * river;
      outriver = blitz::where(river < 0.5 && lastriver > 0.5, 1.0, 0);
      strand = blitz::where(outriver > 0.5 && *(isdry) > 0.5, 1.0, 0.0);
      strand *= *(cell_area);
      entrap = blitz::where(outriver > 0.5 && *(isdry) < 0.5, 1.0, 0.0);
      entrap *= *(cell_area);

      for (int q = qmin; q <= qmax && !first; ++q) {
        if (qarea[q] > 0.0) {
          double qriver(blitz::sum(blitz::where(quad == q, river_area, 0.0)));
          double qstrand(blitz::sum(blitz::where(quad == q, strand, 0.0)));
          double qentrap(blitz::sum(blitz::where(quad == q, entrap, 0.0)));
          std::cout << boost::str(boost::format("%s %3d %10.6e %10.6e %10.6e") %
                                  input->current_sname() %
                                  q %
                                  (qriver*sqft2ha) %
                                  (qstrand*sqft2ha) %
                                  (qentrap*sqft2ha))
                    << std::endl;
        } 
      }
      first = false;
      lastriver = river;
    }

  } catch (const CGNSFile::error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  } catch (const std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }    
  if (f.good()) {
    std::cout.rdbuf(coutbuf);
    f.close();
  }
  

  return 0;
}
