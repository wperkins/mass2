// -------------------------------------------------------------
// file: multi-mixed.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created November  9, 2004 by William A. Perkins
// Last Change: Fri Dec  2 14:44:59 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <sstream>
#include <iomanip>
#include <vector>

#include "cgnsfile.h"

int nblk = 4;
double size = 1.0;
int ncell[] = { 3, 4, 5, 5 };
bool structured[] = { true, false, true, false };

const char *outname = "multi-mixed.cgns";

// -------------------------------------------------------------
// build_unstructured_cube
// -------------------------------------------------------------
void
build_unstructured_cube(CGNSFile& out, const char* name, const int& ncell, 
                        const double& size, const double& x0)
{
  int nvert = (ncell+1)*(ncell+1)*(ncell+1);
  try { 

    std::vector<double> x(nvert);
    std::vector<double> y(nvert);
    std::vector<double> z(nvert);

    int i, j, k;
    int n = 0;

    for (k = 0; k < ncell+1; k++) {
      for (j = 0; j < ncell+1; j++) {
        for (i = 0; i < ncell+1; i++, n++) {
          x[n] = x0 + ((double)i/(double)ncell)*size;
          y[n] = ((double)j/(double)ncell)*size;
          z[n] = ((double)k/(double)ncell)*size;
        } 
      }
    }

    cgns::cgsize_t zsize[3] = {
      nvert,  
      ncell*ncell*ncell,
      0 };
    
    CGNSFile::ZonePtr zone(out.zonewrite(name, &zsize[0], cgns::Unstructured));

    out.coordwrite("CoordinateX", &x[0]);
    out.coordwrite("CoordinateY", &y[0]);
    out.coordwrite("CoordinateZ", &z[0]);

    CGNSFile::SectionPtr sect(out.newsection());

    sect->name = "Hex Cells";
    sect->type = cgns::HEXA_8;
    sect->idxbeg = 1;
    sect->idxend = ncell*ncell*ncell;
    sect->nbnd = 0;

    int cvert = out.npe(sect->type);
    cgns::cgsize_t cmap[cvert*ncell*ncell*ncell];

    n = 0;
    for (int k = 0; k < ncell; k++) {
      for (int j = 0; j < ncell; j++) {
        for (int i = 0; i < ncell; i++) {
          cmap[n+0] = i + j*(ncell+1) + k*(ncell+1)*(ncell+1) + 1;
          cmap[n+1] = (i+1) + j*(ncell+1) + k*(ncell+1)*(ncell+1) + 1;
          cmap[n+2] = (i+1) + (j+1)*(ncell+1) + k*(ncell+1)*(ncell+1) + 1;
          cmap[n+3] = i + (j+1)*(ncell+1) + k*(ncell+1)*(ncell+1) + 1;
          cmap[n+4] = i + j*(ncell+1) + (k+1)*(ncell+1)*(ncell+1) + 1;
          cmap[n+5] = (i+1) + j*(ncell+1) + (k+1)*(ncell+1)*(ncell+1) + 1;
          cmap[n+6] = (i+1) + (j+1)*(ncell+1) + (k+1)*(ncell+1)*(ncell+1) + 1;
          cmap[n+7] = i + (j+1)*(ncell+1) + (k+1)*(ncell+1)*(ncell+1) + 1;
          n += cvert;
        }
      }
    }
    out.sectionwrite(sect, cmap);

                                // Make some boundary conditions.

    CGNSFile::BCPtr wall(out.newbc());
    std::vector<cgns::cgsize_t> ptlist;
    ptlist.reserve((ncell+1)*(ncell+1) + (ncell)*(ncell+1) + 
                   (ncell)*(ncell+1) + (ncell)*(ncell+1));

    wall->name = zone->name + " Wall";
    wall->type = cgns::BCWall;
    wall->pttype = cgns::PointList;
    n = 1;
    for (int k = 0; k < ncell+1; k++) {
      for (int j = 0; j < ncell+1; j++) {
        for (int i = 0; i < ncell+1; i++, n++) {
          if (j == 0 || k == 0 || j == ncell || k == ncell) {
            ptlist.push_back(n);
          }
        }
      }
    }

    wall->npts = ptlist.size();
    out.bcwrite(wall, &ptlist[0]);
    
  } catch (const CGNSFile::error& e) { 
    throw e; 
  }

}



// -------------------------------------------------------------
// build_structured_cube
// -------------------------------------------------------------
void
build_structured_cube(CGNSFile& out, const char* name, const int& ncell, 
                      const double& size, const double& x0)
{
  try { 

    int nvert = (ncell+1)*(ncell+1)*(ncell+1);
    std::vector<double> x(nvert);
    std::vector<double> y(nvert);
    std::vector<double> z(nvert);

    int i, j, k;

    int n = 0;
    for (k = 0; k < ncell+1; k++) {
      for (j = 0; j < ncell+1; j++) {
        for (i = 0; i < ncell+1; i++, n++) {
          x[n] = x0 + ((double)i/(double)ncell)*size;
          y[n] = ((double)j/(double)ncell)*size;
          z[n] = ((double)k/(double)ncell)*size;
        } 
      }
    }

    cgns::cgsize_t zsize[3][3] = {
      { ncell + 1, ncell + 1, ncell + 1 },  
      { ncell,     ncell,     ncell     },  
      { 0,         0,         0         }};
    
    CGNSFile::ZonePtr zone(out.zonewrite(name, &zsize[0][0], cgns::Structured));

    out.coordwrite("CoordinateX", &x[0]);
    out.coordwrite("CoordinateY", &y[0]);
    out.coordwrite("CoordinateZ", &z[0]);


                                // Add a wall boundary condition

    CGNSFile::BCPtr wall(out.newbc());
    std::vector<cgns::cgsize_t> ptlist;
    ptlist.reserve(3*((ncell+1)*(ncell+1) + (ncell)*(ncell+1) + 
                      (ncell)*(ncell+1) + (ncell)*(ncell+1)));

    wall->name = zone->name + " Wall";
    wall->type = cgns::BCWall;
    wall->pttype = cgns::PointList;

    for (k = 0; k < ncell+1; k++) {
      for (j = 0; j < ncell+1; j++) {
        for (i = 0; i < ncell+1; i++) {
          if (j == 0 || k == 0 || j == ncell || k == ncell) {
            ptlist.push_back(i+1);
            ptlist.push_back(j+1);
            ptlist.push_back(k+1);
          }
        } 
      }
    }
    wall->npts = ptlist.size()/3;
    out.bcwrite(wall, &ptlist[0]);

    
  } catch (const CGNSFile::error& e) { 
    throw e; 
  }
}

// -------------------------------------------------------------
// connect_blocks
// -------------------------------------------------------------
/** 
 * This connects two blocks.  This is in no way general.  I relies on
 * the knowledge that blocks are connected in a particular fashion and
 * order.  Unless the connection in 1-to-1 the donor information of
 * the connection is useless.  I'm am unsure how it is supposed to
 * work, especially the interpolant data.  
 *
 * It appears that one cannot connect a vertex list to a vertex list.
 * One can connect a vertex list to a cell list where a donor cell is
 * specified for each vertex.
 * 
 * @param out 
 * @param name 
 * @param westidx 
 * @param eastidx 
 */
void
connect_blocks(CGNSFile& out, const char* name, 
               const int& westidx, const int& eastidx)
{
  CGNSFile::ZonePtr westzone, eastzone;
  CGNSFile::ConnPtr westconn, eastconn;

  try {
    westzone = out.zoneread(westidx+1);
    westconn = out.newconn();
    eastzone = out.zoneread(eastidx+1);
    eastconn = out.newconn();
  } catch (const CGNSFile::error& e) { 
    throw e; 
  }

  int wsize = ncell[westidx]+1;
  int esize = ncell[eastidx]+1;

  westconn->name = name;
  westconn->donorname = eastzone->name;
  westconn->donorconnname = name;
  westconn->donor_zonetype = eastzone->type;

  eastconn->name = name;
  eastconn->donorname = westzone->name;
  eastconn->donorconnname = name;
  eastconn->donor_zonetype = westzone->type;

  if (wsize == esize) {
    westconn->conntype = cgns::Abutting1to1;
    eastconn->conntype = cgns::Abutting1to1;
  } else {
    westconn->conntype = cgns::Abutting;
    eastconn->conntype = cgns::Abutting;
  }

  cgns::cgsize_t wlist[wsize*wsize*3];     // should be maximum size
  cgns::cgsize_t elist[esize*esize*3];
  cgns::cgsize_t wdlist[wsize*wsize*3];
  cgns::cgsize_t edlist[wsize*wsize*3];
  double winterp[wsize*wsize*3];
  double einterp[wsize*wsize*3];
  int i, j, k, n, idx;

  westconn->ptsettype = cgns::PointList;
  westconn->npnts = wsize*wsize;
  if (wsize == esize) {
    westconn->donor_ptset_type = cgns::PointListDonor;
  } else {
    westconn->donor_ptset_type = cgns::CellListDonor;
  }
  westconn->ndata_donor = wsize*wsize;

  switch (westzone->type) {
  case (cgns::Structured):
    n = 0;
    for (k = 0; k < wsize; k++) {
      for (j = 0; j < wsize; j++) {
        for (i = 0; i < wsize; i++) {

                                // record the vertexes on the east
                                // side of the west block

          if (i == wsize-1) {
            wlist[n+0] = i+1;
            wlist[n+1] = j+1;
            wlist[n+2] = k+1;

            if (wsize == esize) {

                                // If there is a 1-to-1 connection
                                // record the same vertexes in the
                                // donor list for the east block.

              edlist[n+0] =  wlist[n+0];
              edlist[n+1] =  wlist[n+1];
              edlist[n+2] =  wlist[n+2];

            } else {
                                // Otherwise, build a dummy donor list
                                // with fictitious cell indexes and
                                // dummy interpolants

              switch (westconn->donor_zonetype) {
              case (cgns::Structured):
                wdlist[n+0] = 1; // supposed to be a cell index
                wdlist[n+1] = 1;
                wdlist[n+2] = 1;
                break;
              case (cgns::Unstructured):
                wdlist[n/3] = 1;
                break;
              default:
                break;
              }
              winterp[n/3] = 1.0;
            }
            n += 3;
          }
        }
      }
    }
    break;
  case (cgns::Unstructured):
    n = 1;
    idx = 0;
    for (k = 0; k < wsize; k++) {
      for (j = 0; j < wsize; j++) {
        for (i = 0; i < wsize; i++, n++) {

                                // record the vertexes on the east
                                // side of the west block

          if (i == wsize-1) { 
            wlist[idx] = n;

                                // If there is a 1-to-1 connection
                                // record the same vertexes in the
                                // donor list for the east block.

            if (wsize == esize) {
              edlist[idx] = n;
            } else {
                                // Otherwise, build a donor list
                                // with fictitious cell indexes and
                                // dummy interpolants

              switch (westconn->donor_zonetype) {
              case (cgns::Structured):
                wdlist[3*idx+0] = 1;
                wdlist[3*idx+1] = 1;
                wdlist[3*idx+2] = 1;
                break;
              case (cgns::Unstructured):
                wdlist[idx] = 1;
                break;
              default:
                break;
              }
              winterp[idx] = 1.0;
            }
            idx++;
          }
        }
      }
    }
    break;
  default:
    break;
  }

  eastconn->ptsettype = cgns::PointList;
  eastconn->npnts = esize*esize;
  if (esize == wsize) {
    eastconn->donor_ptset_type = cgns::PointListDonor;
  } else {
    eastconn->donor_ptset_type = cgns::CellListDonor;
  }
  eastconn->ndata_donor = esize*esize;

  switch (eastzone->type) {
  case (cgns::Structured):
    n = 0;
    for (k = 0; k < esize; k++) {
      for (j = 0; j < esize; j++) {
        for (i = 0; i < esize; i++) {
          if (i == 0) {

            elist[n+0] = i+1;
            elist[n+1] = j+1;
            elist[n+2] = k+1;

            if (esize == wsize) {

                                // If there is a 1-to-1 connection
                                // record the same vertexes in the
                                // donor list for the east block.

              wdlist[n+0] = elist[n+0];
              wdlist[n+1] = elist[n+1];
              wdlist[n+2] = elist[n+2];

            } else {
                                // Otherwise, build a dummy donor list
                                // with fictitious cell indexes and
                                // dummy interpolants
              switch (eastconn->donor_zonetype) {
              case (cgns::Structured):
                edlist[n+0] = 1; // supposed to be a cell indexes
                edlist[n+1] = 1;
                edlist[n+2] = 1;
                break;
              case (cgns::Unstructured):
                edlist[n/3] = 1; // supposed to be a cell index
                break;
              default:
                break;
              }
              einterp[n/3] = 1.0;
            }
            n += 3;
          }
        }
      }
    }
    break;
  case (cgns::Unstructured):
    n = 1;
    idx = 0;
    for (k = 0; k < esize; k++) {
      for (j = 0; j < esize; j++) {
        for (i = 0; i < esize; i++, n++) {
          if (i == 0) {
            elist[idx] = n;
            if (esize == wsize) {

                                // If there is a 1-to-1 connection
                                // record the same vertexes in the
                                // donor list for the east block.

              wdlist[idx] = n;

            } else {
                                // Otherwise, build a donor list
                                // with fictitious cell indexes and
                                // dummy interpolants

              switch (eastconn->donor_zonetype) {
              case (cgns::Structured):
                edlist[3*idx+0] = 1;
                edlist[3*idx+1] = 1;
                edlist[3*idx+2] = 1;
                break;
              case (cgns::Unstructured):
                edlist[idx] = 1;
                break;
              default:
                break;
              }
              einterp[idx] = 1.0;
            }
            idx++;
          }
        }
      }
    }
    break;
  default:
    break;
  }

  try {
    out.connwrite(westconn, wlist, wdlist, winterp);
    out.connwrite(eastconn, elist, edlist, einterp);
  } catch (const CGNSFile::error& e) { 
    throw e; 
  }

}

// -------------------------------------------------------------
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{
  CGNSFile out;
  int i;
  double x0;
  try {
    out.open(outname, CG_MODE_WRITE);
    out.basewrite("Base", 3, 3);
    for (i = 0, x0 = 0.0; i < nblk; i++) {
      std::ostringstream buf;
      buf << "Zone" << std::setfill('0') << std::setw(2) << i;
      if (structured[i]) {
        build_structured_cube(out, buf.str().c_str(), ncell[i], size, x0);
      } else {
        build_unstructured_cube(out, buf.str().c_str(), ncell[i], size, x0);
      }
      x0 += size;
    }
    out.close();
  } catch (const std::bad_alloc& e) {
    std::cerr << "CGNS Error: " << e.what() << std::endl;
    exit(3);
  } 
  try {
    out.open(outname, CG_MODE_MODIFY);
    for (i = 0; i < nblk - 1; i++) {
      std::ostringstream sbuf;
      sbuf << "Connect " << i << ":" << i+1;
      connect_blocks(out, sbuf.str().c_str(), i, i+1);
    }
    out.close();
  } catch (const CGNSFile::error& e) {
    std::cerr << "CGNS Error: " << e.what() << std::endl;
    exit(3);
  } 
  return 0;
}
