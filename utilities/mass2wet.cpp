// -------------------------------------------------------------
// file: mass2wet.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created November 28, 2018 by William A. Perkins
// Last Change: 2018-12-03 10:09:02 d3g096
// -------------------------------------------------------------


#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>
#include <list>
#include <boost/program_options.hpp>
#include <boost/utility.hpp>
#include <boost/format.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/shared_ptr.hpp>
#include "boost/date_time/posix_time/posix_time.hpp"
#include <boost/spirit/include/classic.hpp>

#include "mass2series.h"
#include "mybasename.h"
#include "DateTime.h"

std::string program("unknown");

typedef boost::gregorian::date Date;
typedef boost::posix_time::ptime DateTime;
typedef boost::posix_time::time_duration Duration;
typedef std::vector<std::string> svector;

const char the_datetime_fmt[] = "%m-%d-%Y %H:%M:%S";

std::string thesep(", ");

// -------------------------------------------------------------
//  class WetEvent
// -------------------------------------------------------------
class WetEvent
  : private boost::noncopyable
{
protected:

  /// Event start time
  DateTime my_start;

  /// Event end time
  DateTime my_end;

  /// Number of time steps
  int my_n;

  /// Depth (in m) accumulator
  double my_depth;

  /// Maximum depth (in m)
  double my_depth_max;

  /// Water surface elevation (in m) accumulator
  double my_wsel;

  /// Minimum water surface elevation (in m)
  double my_wsel_min;

  /// Maximum water surface elevation (in m)
  double my_wsel_max;

  /// Temperature (C) accumulator
  double my_temp;

  /// Minimum temperature (C)
  double my_temp_min;

  /// Maximum temperature (C)
  double my_temp_max;

public:

  /// Default constructor.
  WetEvent(const DateTime& start, const double& d, const double& el, const double& T)
    : my_start(start), my_end(start),
      my_n(1), my_depth(d*0.3048), my_depth_max(my_depth),
      my_wsel(el*0.3048), my_wsel_min(my_wsel), my_wsel_max(my_wsel),
      my_temp(T), my_temp_min(T), my_temp_max(T)
  {}

  /// Destructor
  ~WetEvent(void)
  {}

  /// Continue this event (if it's wet)
  void update(const DateTime& date,
              const double& depth, const double& wsel, const double& T)
  {
    my_end = date;
    my_depth += depth*0.3048;
    my_depth_max = std::max(my_depth_max, depth);
    my_wsel += wsel*0.3048;
    my_wsel_min = std::min(my_wsel_min, wsel*0.3048);
    my_wsel_max = std::max(my_wsel_max, wsel*0.3048);

    my_temp += T;
    my_temp_min = std::min(my_temp_min, T);
    my_temp_max = std::max(my_temp_max, T);

    my_n++;
  }

  /// Thing to output a WetEvent instance (in its current state)
  friend std::ostream& operator<< (std::ostream& stream, const WetEvent& event);
};

typedef boost::shared_ptr<WetEvent> WetEventPtr;

// -------------------------------------------------------------
// struct Sample
// -------------------------------------------------------------
struct Sample {
  int i;
  int j;
  double X, Y;
  WetEventPtr event;
};

typedef boost::shared_ptr<Sample> SamplePtr;
typedef std::list<SamplePtr> SampleList;

// -------------------------------------------------------------
// operator<<
// -------------------------------------------------------------
std::ostream&
operator<<(std::ostream& stream, const WetEvent& event)
{
  Duration dur(event.my_end - event.my_start);
  double avgdepth(event.my_depth/static_cast<double>(event.my_n));
  double avgwsel(event.my_wsel/static_cast<double>(event.my_n));
  double avgtemp(event.my_temp/static_cast<double>(event.my_n));

  double days(dur.total_seconds()/86400.0);
  static const int fw(10);

  stream << event.my_start << thesep
         << event.my_end << thesep
         << std::fixed << std::setprecision(3) 
         << std::setw(fw) << days << thesep
         << std::setw(fw) << avgdepth << thesep
         << std::setw(fw) << event.my_depth_max << thesep
         << std::setw(fw) << event.my_wsel_min << thesep
         << std::setw(fw) << avgwsel << thesep
         << std::setw(fw) << event.my_wsel_max << thesep
         << std::setw(fw) << event.my_temp_min << thesep
         << std::setw(fw) << avgtemp << thesep
         << std::setw(fw) << event.my_temp_max;
  return stream;
}

std::ostream&
operator<<(std::ostream& stream, const Sample& s)
{
  stream << std::setw(8) << s.i << thesep
         << std::setw(8) << s.j << thesep
         << std::fixed << std::setw(15) << std::setprecision(3)
         << s.X << thesep
         << s.Y << thesep
         << *(s.event) << std::endl;
  return stream;
}

// -------------------------------------------------------------
// header
// -------------------------------------------------------------
void
header(std::ostream& stream)
{
  stream << "i" << thesep
         << "j" << thesep
         << "easting" << thesep
         << "northing" << thesep
         << "wetstart" << thesep
         << "wetend" << thesep
         << "wetdays" << thesep
         << "depthavg" << thesep
         << "depthmax" << thesep
         << "wselmin" << thesep
         << "wselavg" << thesep
         << "wselmax" << thesep
         << "tempmin" << thesep
         << "tempavg" << thesep
         << "tempmax" << thesep
         << std::endl;
}

// -------------------------------------------------------------
// read_samples
// -------------------------------------------------------------
void
read_samples(const std::string& name, SampleList& samples)
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

  std::string sname;
  int sindex;

  int i, j;
  double X, Y;

  chlit<> dq = ch_p('"');
  chlit<> sq = ch_p('\'');
  chlit<> comma = ch_p(',');

  // this should match a comment or a blank line
  rule <phrase_scanner_t> comment = ( ( '#' >> *(anychar_p) >> end_p)  || end_p );
    
  // this rule defines the record format
  rule <phrase_scanner_t> fldrule =

                                // MASS2 cell indexes i and j

    int_p[assign_a(i)] >> comma >>
    int_p[assign_a(j)] >> comma >>

                                // Coordinates

    real_p[assign_a(X)] >> comma >>
    real_p[assign_a(Y)] >> 
  
                                // there may be a comment or just the end
    
    ( ( '#' >> *anychar_p >> end_p)  || end_p );

  int ierr = 0;
  int lnum = 0;
  char cbuf[1024];

  f.getline(cbuf, sizeof(cbuf)); // skip header line

  header(std::cout);
  
  while (f) {

    f.getline(cbuf, sizeof(cbuf));
    lnum++;

    parse_info<const char *> presult;

    //  initialize
    sname.clear();
    sindex = -1;

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

      SamplePtr s(new Sample);
      s->i = i;
      s->j = j;
      s->X = X;
      s->Y = Y;
      samples.push_back(s);
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
  std::string samplename;
  std::string listname;
  std::string outname;
  bool verbose;
  int thebase;
  int thezone;

  bool doskip(true);

  try {

    desc.add_options()
      ("help", "produce this help message")
      ("verbose", "produce some diagnostic messages")
      ("base", po::value<int>()->default_value(1), "CGNS base node index")
      ("zone", po::value<int>()->default_value(1), 
       "Index of zone whose solutions should be used")
      ("samples", po::value<std::string>()->default_value(""),
       "CSV file containg list of (0-based) MASS2 cell indexes (i, j, X, Y) to sample")
      ("solution-list",  po::value<std::string>()->default_value(""),
       "Read file/solution list from named file")
      ("cgns-input", po::value< svector >(), "input CGNS file(s)")
      ("output", po::value<std::string>(),
       "Write statistics output to specified file")
      ;
    
    po::positional_options_description p;
    p.add("samples", 1).add("cgns-input", -1);
    
    po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
    po::notify(vm);    
    
    
    if (vm.count("help")) {
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << std::endl;
      return 1;
    }

    samplename = vm["samples"].as<std::string>();

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

  boost::posix_time::time_facet *output_facet =
    new boost::posix_time::time_facet(mass2_datetime_fmt);
  std::cout.imbue(std::locale(std::locale(), output_facet));
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

    if (verbose) {
      std::cerr << program << ": info: reading samples from "
                << samplename << std::endl;
    }
    SampleList samples;
    read_samples(samplename, samples);
    
    boost::scoped_ptr<MASS2Series> input;
    if (!listname.empty()) {
      input.reset(new MASS2Series(listname));
    } else {
      input.reset(new MASS2Series(inname));
    }

    std::string lastfile("bogus");
    std::string lastsoln("bogus");
    bool first(true);
    
    while (input->advance()) {

      int isol(input->current_sindex());
      std::string sname(input->current_sname());

      DateTime thedate(strp_datetime(sname.c_str(), mass2_datetime_fmt));
      
      if (lastfile != input->current_name()) {
        lastfile = input->current_name();
        if (verbose) {
          std::cerr << program << ": info: processing CGNS file " 
                    << "\"" << lastfile << "\""
                    << std::endl;
        }
      }

      MASS2Solution::CellFieldPtr isdry(input->get_cell_field("isdry"));
      MASS2Solution::CellFieldPtr depth(input->get_cell_field("depth"));
      MASS2Solution::CellFieldPtr wsel(input->get_cell_field("wsel"));
      MASS2Solution::CellFieldPtr temp(input->get_cell_field("temperature"));


      for (SampleList::iterator s = samples.begin(); s != samples.end(); ++s){
        double d((*depth)((*s)->j,(*s)->i));
        double el((*wsel)((*s)->j,(*s)->i));
        double t((*temp)((*s)->j, (*s)->i));
        if ((*isdry)((*s)->j,(*s)->i) < 0.5) { // not dry = wet
          if ((*s)->event) {
            (*s)->event->update(thedate, d, el, t);
          } else {
            (*s)->event.reset(new WetEvent(thedate, d, el, t));
          }
        } else {                // dry
          if ((*s)->event) {
            std::cout << *(*s);
            (*s)->event.reset();
          }
        }
      }
    }

    // Close out any events that are still active

    for (SampleList::iterator s = samples.begin(); s != samples.end(); ++s) {
      if ((*s)->event) {
        std::cout << *(*s);
        (*s)->event.reset();
      }
    }
    samples.clear();
      
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

