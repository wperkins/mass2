// -------------------------------------------------------------
// file: mass2grass.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created October 25, 2017 by William A. Perkins
// Last Change: 2018-06-08 09:50:32 d3g096
// -------------------------------------------------------------


#include <iostream>
#include <fstream>
#include <vector>
#include <boost/program_options.hpp>
#include <boost/format.hpp>
#include <boost/scoped_ptr.hpp>

#include "mass2series.h"
#include "mybasename.h"

std::string program("unknown");
typedef std::vector<std::string> svector;


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
  std::string inname;
  std::string outname;
  svector field;
  bool verbose;
  int thebase;
  int thezone;
  int thesol;
  char thesep('|');
  double xmin, xmax, ymin, ymax;
  bool header;
  bool doall;

  try {

    desc.add_options()
      ("help", "produce this help message")
      ("verbose", "produce some diagnostic messages")
      ("base", po::value<int>()->default_value(1), "CGNS base node index")
      ("zone", po::value<int>()->default_value(1), 
       "Index of zone whose solutions should be used")
      ("solution", po::value<int>()->default_value(1),
       "CGNS solution index to extract")
      ("field", po::value< svector >(), "Solution field(s) of interest")
      ("all", "Extract all fields from the specified solution")
      ("header", "write a header with column names to output")
      ("xmin", po::value<double>()->default_value(-1.0E+30),
       "Lower limit output x (east) coordinates")
      ("xmax", po::value<double>()->default_value(1.0E+30),
       "Upper limit output x (east) coordinates")
      ("ymin", po::value<double>()->default_value(-1.0E+30),
       "Lower limit output y (north) coordinates")
      ("ymax", po::value<double>()->default_value(1.0E+30),
       "Upper limit output y (north) coordinates")
      ("separator", po::value<char>()->default_value(thesep),
       "Field separator charactor")
      ("cgns-input", po::value< std::string >(), "input MASS2 CGNS file")
      ("output", po::value<std::string>(),
       "Write output to named file")
      ;
    
    po::positional_options_description p;
    p.add("cgns-input", 1).add("field", -1);
    
    po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
    po::notify(vm);    
    
    
    if (vm.count("help")) {
      std::cerr << program << ": extract MASS2 solution in GRASS point format" << std::endl;
      std::cerr << "Usage: " << program << " [options] [ file.cgns [field] [field] ... " << std::endl;
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

    verbose = (vm.count("verbose") > 0); 
    thebase = vm["base"].as<int>();
    thezone = vm["zone"].as<int>();
    thesol = vm["solution"].as<int>();
    thesep = vm["separator"].as<char>();

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

    if (vm.count("output") > 0) {
      outname = vm["output"].as<std::string>();
    } else {
      outname.clear();
    }

    xmin = vm["xmin"].as<double>();
    xmax = vm["xmax"].as<double>();
    ymin = vm["ymin"].as<double>();
    ymax = vm["ymax"].as<double>();

    header = (vm.count("header") > 0);

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
    MASS2Solution::CellVectorFieldPtr centroid;
    svector innames;
    innames.push_back(inname);
    input.reset(new MASS2Series(innames));

    input->base(thebase);
    input->zone(thezone);

    for (int i = 1; i <= thesol; i++) {
      if (!input->advance()) {
        std::cerr << program << ": error: " << input->current_name() 
                  << ": cannot advance to solution "
                  << i
                  << std::endl;
        throw std::runtime_error("bad solution index");
      }
    }
    centroid = input->get_cell_coordinates();

    if (doall) {
      field.clear();
      const MASS2Solution *s(input->current());
      int sidx(input->current_sindex());
      int nfld = s->nfields(sidx);
      for (int f = 1; f <= nfld; ++f) {
        field.push_back(s->fieldinfo(sidx, f));
      }
    }

    std::vector< MASS2Solution::CellFieldPtr > datafields(field.size());

    for (int i = 0; i < field.size(); ++i) {
      MASS2Solution::CellFieldPtr f(input->get_cell_field(field[i]));
      datafields[i] = f;
    }

    if (header) {
      std::cout << "# " << input->current_sname() << std::endl;
      std::cout << boost::str(boost::format("%3.3s%c%5.5s%c%5.5s%c%15.15s%c%15.15s") %
                              "blk" % thesep % 
                              "i" % thesep % 
                              "j" % thesep % 
                              "easting" % thesep % 
                              "northing");

      for (svector::const_iterator itr = field.begin(); itr != field.end(); ++itr) {
        std::cout << boost::str(boost::format("%c%15.15s") % thesep % *itr);
      }
      std::cout << std::endl;
    }

    blitz::TinyVector<int, 2> shape(centroid->shape());
    for (int i = 0; i < shape[0]; ++i) {
      for (int j = 0; j < shape[1]; ++j) {
        double cx((*centroid)(i,j)[0]);
        double cy((*centroid)(i,j)[1]);

        std::cout << boost::str(boost::format("%3d%c%5d%c%5d%c%15.10g%c%15.10g") %
                                thezone % thesep % j % thesep % i 
                                % thesep % cx % thesep % cy);
        for (int f = 0; f < datafields.size(); ++f) {
          double v((*(datafields[f]))(i, j));
          std::cout << boost::str(boost::format("%c%15.10g") % thesep % v);
        }
        std::cout << std::endl;
      }
    }

  } catch (const CGNSFile::error &e) {
    std::cerr << program << ": error: " << e.what()<< std::endl;
    exit(3);
  }

  if (f.good()) f.close();
  
  return 0;
}
