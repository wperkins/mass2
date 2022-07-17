// -------------------------------------------------------------
// file: cgnscale.cpp
/**
 * @file   cgnscale.cpp
 * @author William A. Perkins
 * @date Fri Dec  2 14:31:04 2011
 * 
 * @brief  A program to do simple scaling of CGNS coordinates and variables.
 * 
 * 
 */

// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created September  9, 2005 by William A. Perkins
// Last Change: Fri Dec  2 14:31:04 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";


#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <algorithm>
#include <cstdlib>
#include <unistd.h>

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include "cgnsfile.h"

static std::string program;

// -------------------------------------------------------------
// usage
// -------------------------------------------------------------
static void
usage(void)
{
  std::cerr << "Usage: " << program 
            << "[-v] -C | -V var cgns factor [offset]"
            << std::endl;
}

// -------------------------------------------------------------
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{
  bool verbose = false;
  int base = 1;
  bool docoord[3] = { false, false, false };
  bool dovar = false;
  std::string varname;
  std::string cgnsname;
  double factor(1.0);
  double offset(0.0);

  program = basename(argv[0]);

                                // handle command line
  int c;

  while (1) {

    c = getopt(argc, argv, "vV:CXYZh?");
    if (c == -1)
      break;

    switch (c) {
    case 'h':
    case '?':
      usage();
      exit(0);
      break;
    case 'v':
      verbose = true;
      break;
    case 'C':
      docoord[0] = true;
      docoord[1] = true;
      docoord[2] = true;
      break;
    case 'X':
      docoord[0] = true;
      docoord[1] = false;
      docoord[2] = false;
      break;
    case 'Y':
      docoord[0] = false;
      docoord[1] = true;
      docoord[2] = false;
      break;
    case 'Z':
      docoord[0] = false;
      docoord[1] = false;
      docoord[2] = true;
      break;
    case 'V':
      dovar = true;
      varname = optarg;
      break;
    default:
      std::cerr << program << "error: bad return value from getopt: " << c << std::endl;
      usage();
      exit(1);
    }
  }

  bool doanycoord(docoord[0] || docoord[1] || docoord[2]);

  if (doanycoord && dovar) {
    std::cerr << program << ": error: specify -C or -V, not both" << std::endl;
    usage();
    exit(1);
  }

  if (!(doanycoord || dovar)) {
    std::cerr << program << ": error: must specify either -C or -V" << std::endl;
    usage();
    exit(1);
  }

  if (optind < argc) {
    cgnsname = argv[optind++];
  } else {
    std::cerr << program << ": error: missing cgns file name" << std::endl;
    usage();
    exit(1);
  }
  
  if (optind < argc) {
    factor = atof(argv[optind++]);
  } else {
    std::cerr << program << ": error: missing scale factor" << std::endl;
    usage();
    exit(1);
  }
  
  if (optind < argc) {
    offset = atof(argv[optind++]);
  } else {
    offset = 0.0;
  }


                                // try to open CGNS file

  try {
    CGNSFile cgns(cgnsname, CG_MODE_MODIFY);

    (void) cgns.base(base);

    int nzone(cgns.zones()), zone;

    for (zone = 1; zone <= nzone; zone++) {
      const CGNSFile::ZonePtr zinfo(cgns.zoneread(zone));
      if (verbose) {
        std::cerr << program << ": info: processing zone " << zone 
                  << " (" << zinfo->name << ")" << std::endl;
      }

      if (docoord[0] || docoord[1] || docoord[2]) {
        int ncoord(cgns.ncoords()), c;
        for (c = 1; c <= ncoord; c++) {
          if (!docoord[c-1]) continue;
          CGNSFile::ArrayPtr cinfo(cgns.coordinfo(c));
          
          std::vector<double> x(cinfo->size());

          if (verbose) {
            std::cerr << program << ": info: reading coordinate " << c
                      << " (" << cinfo->name << ")" << std::endl;
          }
          cgns.coordread(cinfo->name.c_str(), &x[0]);

          if (verbose) {
            std::cerr << program << ": info: scaling "
                      << cinfo->name << " by " << factor << std::endl;
          }
          for (int i = 0; i < cinfo->size(); i++) {
            x[i] *= factor;
            x[i] += offset;
          }

          if (verbose) {
            std::cerr << program << ": info: writing "
                      << cinfo->name << std::endl;
          }

          switch (cinfo->datatype) {
          case (cgns::RealDouble):
          default:
            cgns.coordwrite(cinfo->name.c_str(), &x[0]);
            break;
          case (cgns::RealSingle):
            std::vector<float> xout(cinfo->size());
            std::copy(x.begin(), x.end(), xout.begin());
            cgns.coordwrite(cinfo->name.c_str(), &xout[0], 0, cinfo->datatype);
            break;
          }

          // see if there are any periodic connections and scale the translation (if any)

        }
        int nconn(cgns.nconns());
        for (int c = 1; c <= nconn; c++) {
          CGNSFile::ConnPtr conninfo(cgns.conninfo(c));
          double rc[3], t[3], ra[3];
          
          if (conninfo->periodic) {
            for (int i = 0; i < 3; i++) {
              if (docoord[i]) {
                rc[i] = conninfo->periodic_rcenter[i] * factor;
                rc[i] += offset;
                t[i] = conninfo->periodic_tranlation[i] * factor;
                ra[i] = conninfo->periodic_rotation[i];
              }
            }
            if (verbose) {
              std::cerr << program << ": Zone " << zinfo->name 
                        << ": Connection " << conninfo->name
                        << ": scaling periodic translation from ("
                        << conninfo->periodic_tranlation[0] << ", "
                        << conninfo->periodic_tranlation[1] << ", "
                        << conninfo->periodic_tranlation[2] << ") to ("
                        << t[0] << ", " << t[1] << ", " << t[2] << ")" 
                        << std::endl;
            }
            cgns.connperiodicwrite(c, rc, ra, t);
          }
        }
      } else if (dovar) {
        int nsol(cgns.nsols()), s, slen;
        for (s = 1; s <= nsol; s++) {
          std::string sname(cgns.solinfo(s));
          
          if (verbose) {
            std::cerr << program << ": " << zinfo->name 
                      << ": processing solution "
                      << sname << std::endl;
          }

          switch (cgns.solloc(s)) {
          case (cgns::CellCenter):
            slen = zinfo->cells();
            break;
          case (cgns::Vertex):
            slen = zinfo->vertices();
            break;
          default:
            std::cerr << program << ": error: unknown solution location, " 
                      << cgns.solloc(s) << ", in solution " << s 
                      << " skipping" << std::endl;
            continue;
          } 
          std::vector<double> x(slen);

          int f = cgns.fieldbyname(s, varname.c_str());

          if (verbose) {
            std::cerr << program << ": " << zinfo->name << ": "
                      << sname << ": " << " reading field " 
                      << varname << " (" << f << ")" << std::endl;
          }

          cgns.fieldread(s, varname.c_str(), &x[0]);

          if (verbose) {
            std::cerr << program << ": " << zinfo->name << ": "
                      << sname << ": " << " scaling field " 
                      << varname << " by " << factor << std::endl;
          }
           
          for (int i = 0; i < slen; i++) {
            x[i] *= factor;
            x[i] += offset;
          }

          if (verbose) {
            std::cerr << program << ": " << zinfo->name << ": "
                      << sname << ": " << " writing field " 
                      << varname << std::endl;
          }
          cgns.fieldwrite(s, varname.c_str(), &x[0]);
        }
      }
    }
    cgns.close();
  } catch (const CGNSFile::error &e) {
    std::cerr << program << ": error: " << e.what() << std::endl;
    exit(3);
  }

  return(0);
}

