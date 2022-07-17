// -------------------------------------------------------------
/**
 * @file   cgnstest.cpp
 * @author William A. Perkins
 * @date Fri Dec  2 14:45:00 2011
 * 
 * @brief  A test program for the CGNS file I/O library.
 * 
 * 
 */
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created May 18, 2004 by William A. Perkins
// Last Change: Fri Dec  2 14:45:00 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
// -------------------------------------------------------------

#include <iostream>
#include "cgnsfile.h"

static const char* SCCS_ID = "$Id$ Battelle PNL";

// -------------------------------------------------------------
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{
  CGNSFile myfile;
  char *myfilename;
  
  if (argc <= 1) {
    std::cerr << "Usage: " << argv[0] << " file" << std::endl;
    exit(2);
  }

  myfilename = argv[1];
  

  try {
    myfile.open(myfilename, CG_MODE_READ);
    int nb = myfile.bases();
    std::cout << myfilename << " has " << nb << " base nodes." << std::endl;

    int nz = myfile.zones();
    std::cout << myfilename << " has " << nz << " zones in base node 1." << std::endl;

    for (int z = 1; z <= nz; z++) {
      const CGNSFile::ZonePtr& zone = myfile.zoneread(z);
      int nc = myfile.ncoords();
      std::cout << cgns::ZoneTypeName[zone->type] << " Zone " << z 
           << " (" << zone->name << ")"
           << " has " << zone->vertices() << " vertices" 
           << " and " << zone->cells() << " cells"
           << " and " << nc << " coordinates named:"
           << std::endl;
      double junk[zone->vertices()];
      for (int c = 1; c <= nc; c++) {
        CGNSFile::ArrayPtr a(myfile.coordinfo(c));
        std::cout << "\t" << a->name << "(" ;
        if (a->dim > 0) std::cout << a->dimvector[0];
        if (a->dim > 1) std::cout << ", " << a->dimvector[1];
        if (a->dim > 2) std::cout << ", " << a->dimvector[2];
        std::cout << ")";

        if (a->size() != zone->vertices()) {
          std::cout << " Size incorrect";
        } else {
          std::cout << " Size OK";
        }

        myfile.coordread(a->name.c_str(), &junk[0]);

        std::cout << " Read OK";
        
        std::cout << std::endl;
      }

      if (zone->type == cgns::Unstructured) {
        int ns = myfile.nsections();
        std::cout << cgns::ZoneTypeName[zone->type] << " Zone " << z 
             << " has " << ns << " section(s): " << std::endl;

        for (int s = 1; s <= ns; s++) {
          CGNSFile::SectionPtr sp(myfile.sectionread(s));
          std::cout << "\t" << sp->index << ", \"" << sp->name << "\": " 
               << sp->size() << " elements of type " << cgns::ElementTypeName[sp->type] 
               << " (datasize = " << myfile.elementdatasize(sp) << ")";
          { 
            cgns::cgsize_t edata[myfile.elementdatasize(sp)];
            myfile.elementsread(sp, edata, NULL);
            std::cout << " Read OK";
          }
          
          std::cout << std::endl;
        }
      }

      int nbc = myfile.nbc();
      std::cout << cgns::ZoneTypeName[zone->type] << " Zone " << z 
           << " has " << nbc << " boundary condition(s):" << std::endl;

      for (int ibc = 1; ibc <= nbc; ibc++) {
        CGNSFile::BCPtr bc = myfile.bcinfo(ibc);
        std::cout << "\tBC " << bc->index << " \"" << bc->name << "\": " 
                  << cgns::BCTypeName[bc->type] 
                  << " (" ;
        switch (bc->pttype) {
        case (cgns::PointRange):
          std::cout << "PointRange";
          break;
        case (cgns::PointList):
          std::cout << "PointList";
          break;
        case (cgns::ElementRange):
          std::cout << "ElementRange";
          break;
        case (cgns::ElementList):
          std::cout << "ElementList";
          break;
        default:
          std::cout << "Unknown!";
          break;
        }
         
        std::cout << " with " << bc->npts << " points)" << std::endl;
      }

      int n1to1 = myfile.none2one();
      std::cout << cgns::ZoneTypeName[zone->type] << " Zone " << z 
           << " has " << n1to1 << " one-to-one zone connections" << std::endl;

      int nconn = myfile.nconns();
      std::cout << cgns::ZoneTypeName[zone->type] << " Zone " << z 
           << " has " << nconn << " general zone connections" << std::endl;

      for (int cidx = 1; cidx <= nconn; cidx++) {
        CGNSFile::ConnPtr conn(myfile.conninfo(cidx));
        std::cout << "\t" << conn->name << " ";
        switch (conn->conntype) {
        case (cgns::Overset):
          std::cout << " overlaps ";
          break;
        case (cgns::Abutting):
          std::cout << " abuts ";
          break;
        case (cgns::Abutting1to1):
          std::cout << " abuts (1-to-1) ";
          break;
        default:
          std::cout << " connects somehow to ";
        }
        std::cout << cgns::ZoneTypeName[conn->donor_zonetype] << " zone " 
                  << conn->donorname 
                  << " (" << conn->donorconnname << ") " << std::endl;
        std::cout << "\t\t" << conn->npnts;
        switch (conn->ptsettype) {
        case (cgns::PointList):
          std::cout << " points (list) ";
          break;
        case (cgns::PointRange):
          std::cout << " point (range) ";
          break;
        default:
          std::cout << " (other) ";
        }
        std::cout << " connect to " << conn->ndata_donor;
        switch (conn->donor_ptset_type) {
        case (cgns::PointListDonor):
          std::cout << " points (list) ";
          break;
        case (cgns::CellListDonor):
          std::cout << " cells (list) ";
          break;
        case (cgns::PointRangeDonor):
          std::cout << " point (range) ";
          break;
        default:
          std::cout << " (other = " << conn->donor_ptset_type << ") ";
        }
        cgns::cgsize_t tpt[conn->npnts], dpt[conn->ndata_donor];
        myfile.connread(cidx, tpt, dpt);

        std::cout << " Read OK " << std::endl;
      }

      int nsol = myfile.nsols();
      std::cout << cgns::ZoneTypeName[zone->type] << " Zone " << z 
           << " has " << nsol << " flow solutions(s):" << std::endl;
      
      for (int sidx = 1; sidx <= nsol; sidx++) {
        int nfld = myfile.nfields(sidx);
        std::cout << "\tFlow Solution " << sidx << " \"" << myfile.solinfo(sidx) 
                  << "\" has " << nfld << " data fields:"
                  << std::endl;
        for (int fld = 0; fld < nfld; fld++) {
          std::string fname =  myfile.fieldinfo(sidx, fld+1);
          std::cout << "\t\t" << fname;
          double x[zone->cells()];
          myfile.fieldread(sidx, fname.c_str(), x);
          std::cout << " (Read OK)" << std::endl;
        }
      }
        
    }      

    myfile.close();
  } catch (const CGNSFile::error& e) {
    std::cerr << "error: " << e.what() << std::endl;
    exit(1);
  }
  exit(0);
}
