// -------------------------------------------------------------
// file: copydiscrete.cpp
/**
 * @file   copydiscrete.cpp
 * @author William A. Perkins
 * @date Thu Jun 28 10:07:29 2012
 * 
 * @brief A program to copy all DiscreteData nodes (under Zone) from
 * one CGNS to another.
 * 
 * 
 */

// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created June  3, 2008 by William A. Perkins
// Last Change: Thu Jun 28 10:07:29 2012 by William A. Perkins <d3g096@pe10900.pnl.gov>
// -------------------------------------------------------------

#include <iostream>
#include <string>
#include <boost/program_options.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/format.hpp>

#include "cgnsfile.h"

static const char* SCCS_ID = "$Id$ Battelle PNL";

static std::string program;

// -------------------------------------------------------------
// basename
// -------------------------------------------------------------
const std::string
mybasename(const char* p)
{
  boost::filesystem::path progpath(p);
  std::string bname(progpath.filename().string());
  return bname;
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
  desc.add_options()
    ("help", "produce this help message")
    ("base", po::value<int>()->default_value(1), "CGNS base node index")
    ("cgns-input", po::value< std::string >(), "input CGNS file")
    ("cgns-output", po::value< std::string >(), "output CGNS file")
    ;

  po::positional_options_description p;
  p.add("cgns-input", 1);
  p.add("cgns-output", 1);

  po::variables_map vm;
  po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
  po::notify(vm);    


  int base = vm["base"].as<int>();

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
  
  std::string inname(vm["cgns-input"].as<std::string>());
  std::string outname(vm["cgns-output"].as<std::string>());
  
  CGNSFile in, out;
  
  try {
    in.open(inname.c_str(), CG_MODE_READ);
    out.open(outname.c_str(), CG_MODE_MODIFY);

    CGNSFile::BasePtr inbase = in.baseread(base);
    CGNSFile::BasePtr outbase = out.baseread(base);

    int inzones = in.zones();
    int outzones = out.zones();

    if (inzones != outzones) {
      std::cerr << program << ": error: "
           << (boost::format("inputs zones (%1%) != output zones (%2%)") % inzones % outzones)
           << std::endl;
      exit(2);
    }

    for (int z = 1; z <= inzones; z++) {
      CGNSFile::ZonePtr zone = in.zone(z);
      out.zone(z);

      int ndis = in.ndiscrete();

      if (ndis > 0) {
        std::cerr << program << ": info: "
                  << (boost::format("zone \"%1%\" has %2% discrete data nodes") % zone->name % ndis)
                  << std::endl;
      } else {
        std::cerr << program << ": warning: "
                  << (boost::format("zone \"%1%\" has no discrete data nodes, skipping") % zone->name)
                  << std::endl;
        continue;
      }

      for (int id = 1; id <= ndis; id++) {
        std::string ddname;
        cgns::GridLocation_t ddloc;

        in.discreteread(id, ddname, ddloc);
        int od = out.discretewrite(ddname.c_str(), ddloc);

        int datalen;

        switch (ddloc) {
        case cgns::CellCenter:
          datalen = zone->cells();
          break;
        case cgns::Vertex:
          datalen = zone->vertices();
          break;
        default:
          std::cerr << program << ": error: "
                    << (boost::format("zone %1%: discrete data \"%2%\" has unknown location (%3%)") % zone->name % ddname % ddloc)
                    << std::endl;
          continue;
        }
        
        std::cerr << program << ": info: "
                  << (boost::format("zone %1%: copying discrete data from \"%2%\"") % zone->name % ddname)
                  << std::endl;
        int nfld = in.ndiscretefields(id);

        for (int i = 1; i <= nfld; i++) {
          std::string fname;
          cgns::DataType_t ftype;
          in.discretefieldinfo(id, i, fname, ftype);
          std::cerr << program << ": info: "
                    << (boost::format("zone %1%: %2%: copying field \"%3%\"") % zone->name % ddname % fname)
                    << std::endl;

          switch (ftype) {
          case cgns::RealDouble: 
            {
              std::vector<double> x(datalen);
              in.discretefieldread(id, fname.c_str(), &x[0]);
              out.discretefieldwrite(od, fname.c_str(), ddloc, &x[0]);
            }
            break;
          case cgns::Integer: 
            {
              std::vector<int> x(datalen);
              in.discretefieldread(id, fname.c_str(), &x[0]);
              out.discretefieldwrite(od, fname.c_str(), ddloc, &x[0]);
            }
            break;
          default:
            std::cerr << program << ": error: "
                      << (boost::format("zone %1%: \"%2%\": field \"%3%\" has unknown type (%4%)") % zone->name % ddname % fname % ftype)
                      << std::endl;
            
          }
        }
      }
    }
  } catch (std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }
    
}
