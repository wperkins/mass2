// -------------------------------------------------------------
// file: mass2stats.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created December 15, 2015 by William A. Perkins
// Last Change: 2018-11-29 07:49:34 d3g096
// -------------------------------------------------------------


#include <iostream>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <vector>
#include <map>
#include <boost/program_options.hpp>

#include "mybasename.h"
#include "mass2series.h"

std::string program("unknown");

typedef std::vector<std::string> svector;

typedef std::map<std::string, double> ConversionMap;

// -------------------------------------------------------------
// build_conversion
// -------------------------------------------------------------
ConversionMap
build_conversion(const bool& metric)
{
  ConversionMap result;
  
  if (metric) {
    result["depth"] = 0.3048;     // ft to m
    result["wsel"] = 0.3048;      // ft to m
    result["vmag"] = 0.3048;      // ft/s to m/s
    result["VelocityX"] = 0.3048; // ft/s to m/s
    result["VelocityY"] = 0.3048; // ft/s to m/s
    result["isdry"] = 1.0;
    result["Temperature"] = 1.0;
    result["shear"] = 47.880259;  // lbf/ft2 to N/m2
    result["vstar"] = 0.3048;     // ft/s to m/s
    result["coord"] = 0.3048;
  } else {
    result["depth"] = 1.0;
    result["wsel"] = 1.0;
    result["vmag"] = 1.0;
    result["VelocityX"] = 1.0;
    result["VelocityY"] = 1.0;
    result["isdry"] = 1.0;
    result["Temperature"] = 1.0;
    result["shear"] = 1.0;
    result["vstar"] = 1.0;
    result["coord"] = 1.0;
  }    
  return result;
}


// -------------------------------------------------------------
//  class CountAccumulator
// -------------------------------------------------------------
class CountAccumulator {
protected:

  int n;
  MASS2Solution::CellFieldPtr count;

public:

  /// Default constructor.
  CountAccumulator(const MASS2Solution::Shape& theshape)
    : n(0), 
      count(new MASS2Solution::CellField(theshape))
  {
    *count = 0.0;
  }

  /// Destructor
  ~CountAccumulator(void)
  {}

  /// Take the specified array and accumulate
  void accumulate(MASS2Solution::CellFieldPtr flag)
  {
    *count += *flag;
    n += 1;
  }

  /// Get the total count
  int total(void) const
  {
    return n;
  }

  MASS2Solution::CellFieldPtr
  get_count(void) const
  {
    MASS2Solution::CellFieldPtr 
      result(new MASS2Solution::CellField(*count));
    return result;
  }

  MASS2Solution::Shape
  get_shape(void) const
  {
    return count->shape();
  }

};

typedef boost::shared_ptr<CountAccumulator> CountAccumulatorPtr;


// -------------------------------------------------------------
//  class FieldAccumulator
// -------------------------------------------------------------
class FieldAccumulator {
protected:

  static const int nmoments;

  CountAccumulatorPtr count;
  std::vector<MASS2Solution::CellFieldPtr> sum;
  mutable std::vector<MASS2Solution::CellFieldPtr> zmoment;
  mutable std::vector<MASS2Solution::CellFieldPtr> cmoment;

  MASS2Solution::CellFieldPtr
  get_zmoment(const int& m) const
  {
    int i(m-1);
    if (!zmoment[i]) {
      MASS2Solution::CellFieldPtr cnt(count->get_count());
      zmoment[i].reset(new MASS2Solution::CellField(*(sum[i])));
      *(zmoment[i]) = blitz::where(*cnt > 0.0, (*(zmoment[i]))/(*cnt), 0.0);
    }
    return zmoment[i];
  }
  
public:

  /// Default constructor.
  FieldAccumulator(CountAccumulatorPtr counter)
    : count(counter),
      sum(nmoments), zmoment(nmoments), cmoment(nmoments)
  {
    for (std::vector<MASS2Solution::CellFieldPtr>::iterator s = sum.begin();
         s != sum.end(); ++s) {
      s->reset(new MASS2Solution::CellField(count->get_shape()));
      *(*s) = 0.0;
    }
  }

  /// Destructor
  ~FieldAccumulator(void)
  { }

  /// Take the specified array and accumulate
  void accumulate(MASS2Solution::CellFieldPtr flag,
                  MASS2Solution::CellFieldPtr field)
  {
    MASS2Solution::CellFieldPtr 
      tmp(new MASS2Solution::CellField(*field));
    *tmp *= *flag;
    *sum[0] += *tmp;
    *sum[1] += blitz::pow2(*tmp);
    *sum[2] += blitz::pow3(*tmp);
    *sum[3] += blitz::pow4(*tmp);
  }

  MASS2Solution::CellFieldPtr
  get_moment(const int& m) const
  {
    int i(m-1);
    if (!cmoment[i]) {
      cmoment[i].reset(new MASS2Solution::CellField(*get_zmoment(m)));
      switch (m) {
      case (1):
        break;
      case(2):
        *(cmoment[i]) -= blitz::pow2(*get_zmoment(1));
        break;
      case (3):
        *(cmoment[i]) -= (*get_zmoment(2))*(*get_zmoment(1))*3.0;
        *(cmoment[i]) += blitz::pow3(*get_zmoment(1))*2.0;
        break;
      case (4):
        *(cmoment[i]) -= (*get_zmoment(3))*(*get_zmoment(1))*4.0;
        *(cmoment[i]) += (*get_zmoment(2))*blitz::pow2(*get_zmoment(1))*6.0;
        *(cmoment[i]) -= blitz::pow4(*get_zmoment(1))*3.0;
      }
    }
    return cmoment[i];
  }
};

const int FieldAccumulator::nmoments(4);

typedef boost::shared_ptr<FieldAccumulator> FieldAccumulatorPtr;

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

  svector field;
  bool dowet(false);
  bool doskip(true);
  bool dometric(false);

  double xmin, xmax, ymin, ymax;

  try {

    desc.add_options()
      ("help", "produce this help message")
      ("verbose", "produce some diagnostic messages")
      ("base", po::value<int>()->default_value(1), "CGNS base node index")
      ("zone", po::value<int>()->default_value(1), 
       "Index of zone whose solutions should be used")
      ("wet", "include cell wet count and percent wet fields")
      ("no-skip", "do not skip (adjacent) indentically named solutions")
      ("field", po::value< svector >(), "Solution field(s) of interest")
      ("metric", "Convert output values to SI units")
      ("xmin", po::value<double>()->default_value(-1.0E+30),
       "Lower limit output x (east) coordinates")
      ("xmax", po::value<double>()->default_value(1.0E+30),
       "Upper limit output x (east) coordinates")
      ("ymin", po::value<double>()->default_value(-1.0E+30),
       "Lower limit output y (north) coordinates")
      ("ymax", po::value<double>()->default_value(1.0E+30),
       "Upper limit output y (north) coordinates")
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

    if (vm.count("wet") > 0) dowet = true;
    
    if (vm.count("no-skip") > 0) doskip = false;

    if (vm.count("metric") > 0) dometric = true;

    if (vm.count("output") > 0) {
      outname = vm["output"].as<std::string>();
    }

    xmin = vm["xmin"].as<double>();
    xmax = vm["xmax"].as<double>();
    ymin = vm["ymin"].as<double>();
    ymax = vm["ymax"].as<double>();

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
    
    int scount(0);
    MASS2Solution::CellVectorFieldPtr centroid;
    boost::shared_ptr<CountAccumulator> counter;
    std::map<std::string, FieldAccumulatorPtr> accumulator;

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

    while (input->advance()) {

      if (lastfile != input->current_name()) {
        lastfile = input->current_name();
        if (verbose) {
          std::cerr << program << ": info: processing CGNS file " 
                    << "\"" << lastfile << "\""
                    << std::endl;
        }
      }

      if (!centroid) {
        centroid = input->get_cell_coordinates();
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

      scount += 1;

      MASS2Solution::CellFieldPtr iswet(input->get_cell_field("isdry"));        
      *iswet -= 1.0;
      *iswet *= -1.0;

      if (!counter) {
        counter.reset(new CountAccumulator(iswet->shape()));
        for (svector::const_iterator f = field.begin(); f != field.end(); ++f) {
          accumulator[*f].reset(new FieldAccumulator(counter));
        }
      }
      counter->accumulate(iswet);
      for (svector::const_iterator f = field.begin(); f != field.end(); ++f) {
        MASS2Solution::CellFieldPtr fld;
        if (*f == "vmag") {
          fld = input->get_cell_velocity_magnitude();
        } else if (*f == "vstar") {
          fld = input->get_cell_shear_velocity();
        } else {
          fld = input->get_cell_field(*f);
        }
        accumulator[*f]->accumulate(iswet, fld);
      }
    }

    ConversionMap convert = build_conversion(dometric);

    MASS2Solution::CellFieldPtr nwet(counter->get_count());
    MASS2Solution::CellFieldPtr pctwet(new MASS2Solution::CellField(nwet->shape()));
    *pctwet = *nwet;
    *pctwet /= static_cast<double>(counter->total());
    *pctwet *= 100.0;

    std::cout << "i" << ", " << "j" << ", "
              << "Easting" << ", "
              << "Northing" ;
    if (dowet) {
      std::cout << ", "
                << "NWet" << ", "
                << "WetPercent" ;
    }
    for (svector::const_iterator f = field.begin(); f != field.end(); ++f) {
      std::cout << ", " 
                << *f + "1" << ", " 
                << *f + "2" << ", " 
                << *f + "sd" << ", "
                << *f + "3" << ", " 
                << *f + "4";
    }
    std::cout << std::endl;
    blitz::TinyVector<int, 2> shape(nwet->shape());
    for (int i = 0; i < shape[0]; ++i) {
      for (int j = 0; j < shape[1]; ++j) {
        double cx((*centroid)(i,j)[0]*convert["coord"]);
        double cy((*centroid)(i,j)[1]*convert["coord"]);

        if ((xmin <= cx && cx <= xmax) &&
            (ymin <= cy && cy <= ymax)) {
          std::cout << j << ", " << i << ", "
                    << std::fixed << std::setprecision(3)
                    << cx << ", "
                    << cy ;
          if (dowet) {
            std::cout << ", "
                      << static_cast<int>((*nwet)(i,j)) << ", "
                      << std::setprecision(5)
                      << (*pctwet)(i,j) ;
          }
          std::cout << std::scientific << std::setprecision(5);
          for (svector::const_iterator f = field.begin(); f != field.end(); ++f) {
            double c1(convert[*f]), c2(c1*c1), c3(c1*c1*c1), c4(c2*c2);
            std::cout << ", "
                      << (*(accumulator[*f]->get_moment(1)))(i,j)*c1 << ", "
                      << (*(accumulator[*f]->get_moment(2)))(i,j)*c2 << ", "
                      << sqrt((*(accumulator[*f]->get_moment(2)))(i,j)*c2) << ", "
                      << (*(accumulator[*f]->get_moment(3)))(i,j)*c3 << ", "
                      << (*(accumulator[*f]->get_moment(4)))(i,j)*c4;
          }
          std::cout << std::endl;
        }
      }
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
