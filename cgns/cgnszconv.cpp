// -------------------------------------------------------------
// file: cgnszconv.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created January  9, 2007 by William A. Perkins
// Last Change: 2016-03-24 08:29:13 d3g096
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <iomanip>
#include <vector>
#include <libgen.h>
#include <boost/program_options.hpp>
#include <boost/lambda/lambda.hpp>

#include "cgnsfile.h"

char *program;

// -------------------------------------------------------------
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{
  program = basename(argv[0]);

  // Parse command line options

  namespace po = boost::program_options;

  int base;
  std::string inname, outname;
  int thesol(0);
  po::options_description desc("Available options");

  try {
    desc.add_options()
      ("help", "produce this help message")
      ("base", po::value<int>()->default_value(1), "CGNS base node index")
      ("solution",  po::value<int>()->default_value(0) , 
       "Include solution in conversion (-1 = last)")
      ("cgns-input", po::value< std::string >(), "input CGNS file")
      ("cgns-output", po::value< std::string >(), "output CGNS file")
      ;

    po::positional_options_description p;
    p.add("cgns-input", 1);
    p.add("cgns-output", 1);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
    po::notify(vm);    


    base = vm["base"].as<int>();
    thesol = vm["solution"].as<int>();

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
    inname = vm["cgns-input"].as<std::string>();
  
    if (vm.count("cgns-output") <= 0) {
      std::cerr << program << ": error: missing cgns-output" << std::endl;
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << "\n";
      return 1;
    }
    outname = vm["cgns-output"].as<std::string>();

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

  CGNSFile in, out;
  
  try {
    in.open(inname.c_str(), CG_MODE_READ);
    out.open(outname.c_str(), CG_MODE_WRITE);

    CGNSFile::BasePtr inbase = in.baseread(base);
    CGNSFile::BasePtr outbase = out.basewrite(inbase->name.c_str(), inbase->celldim, inbase->physdim);
    
    int nzones(in.zones());
    for (int iz = 1; iz <= nzones; iz++) {
      CGNSFile::ZonePtr inzone = in.zone(iz);

      std::cerr << program << ": converting structured zone " << iz 
                << " \"" << inzone->name << "\" ..." << std::endl;

      cgns::cgsize_t size[3] = { inzone->vertices(), inzone->cells(), 0 };

      CGNSFile::ZonePtr outzone = out.zonewrite(inzone->name.c_str(), size, cgns::Unstructured);

      int ncoords(in.ncoords());
      std::vector<double> tmp(inzone->vertices());
      for (int ic = 1; ic <= ncoords; ic++) {
        CGNSFile::ArrayPtr cinfo = in.coordinfo(ic);
        in.coordread(ic, &tmp[0]);
        out.coordwrite(cinfo->name.c_str(), &tmp[0]);
      }

      std::vector<cgns::cgsize_t> e(inzone->cells()*in.npe(cgns::HEXA_8));
      int nx(inzone->size[0][0]), ny(inzone->size[0][1]), nz(inzone->size[0][2]);
      int c(0);
      for (int k = 0; k < nz-1; k++) {
        for (int j = 0; j < ny-1; j++) {
          for (int i = 0; i < nx-1; i++) {
            e[c+0] = i + j*(nx) + k*(nx)*(ny);
            e[c+1] = (i+1) + j*(nx) + k*(nx)*(ny);
            e[c+2] = (i+1) + (j+1)*(nx) + k*(nx)*(ny);
            e[c+3] = i + (j+1)*(nx) + k*(nx)*(ny);
            e[c+4] = i + j*(nx) + (k+1)*(nx)*(ny);
            e[c+5] = (i+1) + j*(nx) + (k+1)*(nx)*(ny);
            e[c+6] = (i+1) + (j+1)*(nx) + (k+1)*(nx)*(ny);
            e[c+7] = i + (j+1)*(nx) + (k+1)*(nx)*(ny);
            c += in.npe(cgns::HEXA_8);
          }
        }
      }
      std::for_each(e.begin(), e.end(), ++boost::lambda::_1);

      CGNSFile::SectionPtr s = out.newsection();
      s->name = "Hexahedrons";
      s->type = cgns::HEXA_8;
      s->idxbeg = 1;
      s->idxend = inzone->cells();

      out.sectionwrite(s, &e[0]);

      // Output boundary conditions

      int bcs = in.nbc();
      for (int ib = 1; ib <= bcs; ib++) {
        CGNSFile::BCPtr inbc = in.bcinfo(ib);
        std::cerr << program << ": converting boundary " 
                  << " \"" << inbc->name << "\""
                  << " (" << cgns::BCTypeName[inbc->type] << ")"
                  << std::endl;
        CGNSFile::BCPtr outbc = out.newbc();
        outbc->name = inbc->name;
        outbc->type = inbc->type;
        outbc->pttype = cgns::PointList;
        outbc->location = inbc->location;

        std::vector<cgns::cgsize_t> inpt(3*inbc->npts);
        in.bcread(ib, &inpt[0]);

        switch (inbc->pttype) {
        case (cgns::PointList): 
          {
            std::vector<cgns::cgsize_t> outpt(inbc->npts);
            int n = 0;
            for (int ip = 0; ip < 3*inbc->npts; ip+=3, n++) {
              int idx = (inpt[ip]-1) + (inpt[ip+1]-1)*nx + (inpt[ip+2]-1)*nx*ny + 1;
              outpt[n] = idx;
            }
            outbc->npts = inbc->npts;
            out.bcwrite(outbc, &outpt[0]);
          }
          break;
        case (cgns::PointRange):
          {
            int inbcpts(1);
            for (int ip = 0; ip < 3; ip++) {
              inbcpts *= inpt[ip + 3] - inpt[ip] + 1;
            }
            std::vector<cgns::cgsize_t> outpt(inbcpts);
            int n = 0;
            for (int i = inpt[0]; i <= inpt[0+3]; i++) {
              for (int j = inpt[1]; j <= inpt[1+3]; j++) {
                for (int k = inpt[2]; k <= inpt[2+3]; k++) {
                  int idx = (i-1) + (j-1)*nx + (k-1)*nx*ny + 1;
                  outpt[n] = idx;
                  n++;
                }
              }
            }
            outbc->npts = inbcpts;
            out.bcwrite(outbc, &outpt[0]);
          }
          break;
        default:
          std::cerr << program << ": error: cannot handle BC point set type "
                    << cgns::PointSetTypeName[inbc->pttype] 
                    << std::endl;
          exit(3);
        }
      }

      // Output zone connections

      int conns = in.nconns();
      for (int ic = 1; ic <= conns; ic++) {
        CGNSFile::ConnPtr conn = in.conninfo(ic);
        std::cerr << program << ": converting boundary " 
                  << " \"" << conn->name << "\""
                  << std::endl;
        CGNSFile::ConnPtr outconn = out.newconn();
        outconn->name = conn->name;
        outconn->location = conn->location;
        outconn->conntype = conn->conntype;
        outconn->ptsettype = conn->ptsettype;
        outconn->npnts = conn->npnts;
        outconn->donorname = conn->donorname;
        outconn->donorconnname = conn->donorconnname;
        outconn->donor_zonetype = conn->donor_zonetype;
        outconn->donor_datatype = conn->donor_datatype;
        outconn->ndata_donor = 0;
        outconn->periodic = conn->periodic;
        if (conn->periodic) {
          for (int i = 0; i < 3; i++) {
            outconn->periodic_rcenter[i] = conn->periodic_rcenter[i];
            outconn->periodic_rotation[i] = conn->periodic_rotation[i];
            outconn->periodic_tranlation[i] = conn->periodic_tranlation[i];
          }
        }

        // we'll ignore the donor point list

        std::vector<cgns::cgsize_t> inpt(3*conn->npnts);
        in.connread(ic, &inpt[0], NULL, NULL);

        switch (conn->ptsettype) {
        case (cgns::PointList): 
          {
            std::vector<cgns::cgsize_t> outpt(outconn->npnts);
            int n = 0;
            for (int ip = 0; ip < 3*conn->npnts; ip+=3, n++) {
              int idx = (inpt[ip]-1) + (inpt[ip+1]-1)*nx + (inpt[ip+2]-1)*nx*ny + 1;
              outpt[n] = idx;
            }
            out.connwrite(outconn, &outpt[0], NULL, NULL);
          }
          break;
        case (cgns::PointRange):
          {
            int inbcpts(1);
            for (int ip = 0; ip < 3; ip++) {
              inbcpts *= inpt[ip + 3] - inpt[ip] + 1;
            }
            std::vector<cgns::cgsize_t> outpt(inbcpts);
            int n = 0;
            for (int i = inpt[0]; i <= inpt[0+3]; i++) {
              for (int j = inpt[1]; j <= inpt[1+3]; j++) {
                for (int k = inpt[2]; k <= inpt[2+3]; k++) {
                  int idx = (i-1) + (j-1)*nx + (k-1)*nx*ny + 1;
                  outpt[n] = idx;
                  n++;
                }
              }
            }
            out.connwrite(outconn, &outpt[0], NULL, NULL);
          }
          break;
        default:
          std::cerr << program << ": error: cannot handle ZoneConnectivity point set type "
                    << cgns::PointSetTypeName[conn->ptsettype] 
                    << std::endl;
          exit(3);
        }

      }

      if (thesol != 0) {
        int nsol(in.nsols());
        int s;
        if (thesol < 0) {
          s = nsol;
        } else { 
          s = thesol;
        }
        int nfld(in.nfields(s));
        std::string sname(in.solinfo(s));
        cgns::GridLocation_t sloc(in.solloc(s));
        int sout(out.solwrite(sname.c_str(), sloc));
        for (int f = 1; f <= nfld; ++f) {
          std::string fname(in.fieldinfo(s, f));
          in.fieldread(s, fname.c_str(), &tmp[0]);
          out.fieldwrite(sout, fname.c_str(), &tmp[0]);
        }
      }
    }
    out.close();
      
  } catch (std::runtime_error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }
}

