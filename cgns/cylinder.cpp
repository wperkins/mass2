// -------------------------------------------------------------
/**
 * @file   cylinder.cpp
 * @author William A. Perkins
 * @date Fri Dec  2 14:44:59 2011
 * 
 * @brief Test application of CGNSFile to make an unstructured
 * cylindrical mesh.
 * 
 * 
 */

// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created August  3, 2004 by William A. Perkins
// Last Change: Fri Dec  2 14:44:59 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <vector>
#include <cmath>
#include "cgnsfile.h"

bool docyclic = true;


// -------------------------------------------------------------
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{
  try { 

    std::string zonename("Cylindrical Zone");

    double H = (docyclic ? 2.0 : 20.0);
    double R = 0.05;

    int nR = 16;
    int nH = (docyclic ? 10 : 100);
    int nT = 90;

    double dH = H/nH;
    double dR = R/nR;
    double dT = 360.0/nT;

    int nv = nT*nR*(nH+1) + (nH + 1);
    int nc = nc = nT*nR*nH;

    cgns::cgsize_t size[9];
    size[0] = nv;
    size[1] = nc;
    size[2] = 0;
    size[3] = 0;
    size[4] = 0;
    size[5] = 0;
    size[6] = 0;
    size[7] = 0;
    size[8] = 0;

    CGNSFile cf("cylinder.cgns", CG_MODE_WRITE);

    CGNSFile::BasePtr base = cf.basewrite("Cylinder", 3, 3);
    cf.simulationtypewrite(cgns::NonTimeAccurate);

    double g[3] = { 0.0, 0.0, -9.81};
    cf.gravitywrite(g);

    CGNSFile::ZonePtr zone = cf.zonewrite(zonename.c_str(), size, cgns::Unstructured);

    std::vector<double> x(nv), y(nv), z(nv);
    int n, i;

    n = 0;
    for (int ih = 0; ih < nH+1; ih++) {
      x[n] = 0.0;
      y[n] = 0.0;
      z[n] = ih*dH;
      n += 1;
      for (int ir = 0; ir < nR; ir++) {
        double r = (double)(ir + 1)*dR;
        for (int it = 0; it < nT; it++) {
          double t = M_PI*(double)it*dT/180.0;
          x[n] = r*cos(t);
          y[n] = r*sin(t);
          z[n] = ih*dH;
          n += 1;
        }
      }
    }

    cf.coordwrite("CoordinateX", &x[0]);
    cf.coordwrite("CoordinateY", &y[0]);
    cf.coordwrite("CoordinateZ", &z[0]);

    // write center core section
    { 
      std::vector<cgns::cgsize_t> cmap(nT*nH*cf.npe(cgns::PENTA_6));
    // int *cmap = new int[nT*nH*cf.npe(cgns::PENTA_6)];

      i = 0;
      n = 0;
      for (int ih = 0; ih < nH; ih++) {
        for (int it = 0; it < nT; it++) {
          int toff = it + 2;
          if (it + 1 >= nT) toff = 1;
          cmap[i++] = (nR*nT + 1)*ih + 1;
          cmap[i++] = (nR*nT + 1)*ih + (it + 1) + 1;
          cmap[i++] = (nR*nT + 1)*ih + toff + 1;
          cmap[i++] = (nR*nT + 1)*(ih+1) + 1;
          cmap[i++] = (nR*nT + 1)*(ih+1) + (it + 1) + 1;
          cmap[i++] = (nR*nT + 1)*(ih+1) + toff + 1;
          n++;
        }
      }
      
      CGNSFile::SectionPtr sect = cf.newsection();
      
      sect->name = "Pentagonal Cells";
      sect->type = cgns::PENTA_6;
      sect->idxbeg = 1;
      sect->idxend = n;
      sect->nbnd = 0;
      
      cf.sectionwrite(sect, &cmap[0]);
      
      // delete[] cmap;
    }

    // Write the rest of the mesh in another section

    if (nT*nH*(nR-1) > 0) {

      std::vector<cgns::cgsize_t> cmap((nc - nH*nT)*cf.npe(cgns::HEXA_8));
      // cmap = new int[(nc - nH*nT)*cf.npe(cgns::HEXA_8)];

      i = 0;
      for (int ih = 0; ih < nH; ih++) {
        for (int ir = 0; ir < nR - 1; ir++) {
          for (int it = 0; it < nT; it++) {
            int toff;
            if (it + 1 >= nT) {
              toff = 0;
            } else {
              toff = it + 1;
            }
            cmap[i++] = (nR*nT + 1)*ih + nT*ir + it + 1 + 1;
            cmap[i++] = (nR*nT + 1)*ih + nT*(ir+1) + it + 1 + 1;
            cmap[i++] = (nR*nT + 1)*ih + nT*(ir+1) + (toff) + 1 + 1;
            cmap[i++] = (nR*nT + 1)*ih + nT*ir + (toff) + 1 + 1;
            cmap[i++] = (nR*nT + 1)*(ih+1) + nT*ir + it + 1 + 1;
            cmap[i++] = (nR*nT + 1)*(ih+1) + nT*(ir+1) + it + 1 + 1;
            cmap[i++] = (nR*nT + 1)*(ih+1) + nT*(ir+1) + (toff) + 1 + 1;
            cmap[i++] = (nR*nT + 1)*(ih+1) + nT*ir + (toff) + 1 + 1;
          }
        }
      }
    
      CGNSFile::SectionPtr sect = cf.newsection();
      sect->name = "Hexagonal Cells";
      sect->type = cgns::HEXA_8;
      sect->idxbeg = n+1;
      sect->idxend = nc;
      sect->nbnd = 0;

      cf.sectionwrite(sect, &cmap[0]);
    }

    if (docyclic) {

      std::vector<cgns::cgsize_t> tpts(nR*nT + 1);
      for (int k = 0; k <  nR*nT + 1; k++) {
        tpts[k] = nv -  nR*nT + k;
      }

      std::vector<cgns::cgsize_t> bpts(nR*nT + 1);
      for (int k = 0; k <  nR*nT + 1; k++) {
        bpts[k] = k+1;
      }


      CGNSFile::ConnPtr conn = cf.newconn();
      conn->name = "Bottom";
      conn->location = cgns::Vertex;
      conn->conntype = cgns::Abutting1to1;
      conn->npnts = nR*nT + 1;
      conn->ptsettype = cgns::PointList;
      conn->ndata_donor = nR*nT + 1;
      conn->donorname = zonename;
      conn->donorconnname = "Top";
      conn->donor_zonetype = cgns::Unstructured;
      conn->donor_ptset_type = cgns::PointListDonor;
      conn->periodic = true;
      conn->periodic_tranlation[0] = 0.0;
      conn->periodic_tranlation[1] = 0.0;
      conn->periodic_tranlation[2] = H;

      cf.connwrite(conn, &bpts[0], &tpts[0], NULL);

      conn = cf.newconn();
      conn->name = "Top";
      conn->location = cgns::Vertex;
      conn->conntype = cgns::Abutting1to1;
      conn->npnts = nR*nT + 1;
      conn->ptsettype = cgns::PointList;
      conn->ndata_donor = nR*nT + 1;
      conn->donorname = zonename;
      conn->donorconnname = "Bottom";
      conn->donor_zonetype = cgns::Unstructured;
      conn->donor_ptset_type = cgns::PointListDonor;
      conn->periodic = true;
      conn->periodic_tranlation[0] = 0.0;
      conn->periodic_tranlation[1] = 0.0;
      conn->periodic_tranlation[2] = -H;

      cf.connwrite(conn, &tpts[0], &bpts[0], NULL);

    } else {

      std::vector<cgns::cgsize_t> pts(nR*nT + 1);

      CGNSFile::BCPtr bc = cf.newbc();
      bc->name = "Bottom";
      bc->type = cgns::BCWall;
      bc->pttype = cgns::PointRange;
      bc->npts = 2;
      pts[0] = 1; pts[1] = nR*nT + 1;
      
      cf.bcwrite(bc, &pts[0]);
      
      bc = cf.newbc();
      bc->name = "Top";
      bc->type = cgns::BCWall;
      bc->pttype = cgns::PointList;
      bc->npts = nR*nT + 1;
      
      for (int k = 0; k <  nR*nT + 1; k++) {
        pts[k] = nv -  nR*nT + k;
      }
      
      cf.bcwrite(bc, &pts[0]);
    }

    cf.close();

  } catch (const CGNSFile::error& e) {
    std::cerr << e.what() << std::endl;
    return -1;
  }

  return (0);
}
