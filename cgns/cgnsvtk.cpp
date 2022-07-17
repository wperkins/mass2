// -------------------------------------------------------------
/**
 * @file   cgnsvtk.cpp
 * @author William A. Perkins
 * @date Fri Dec  2 14:45:00 2011
 * 
 * @brief  A program to convert CGNS files to the simple VTK format.
 * 
 * 
 */

// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created August 27, 2004 by William A. Perkins
// Last Change: Fri Dec  2 14:45:00 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <iomanip>
#include <vector>
#include <list>
#include <set>
#include <string>
#include <libgen.h>
#include <boost/program_options.hpp>
#include <boost/algorithm/string.hpp>

#include "cgnsfile.h"

char *program;

// -------------------------------------------------------------
// avtk_header
// -------------------------------------------------------------
void
avtk_header(std::ostream& fout, const char* name)
{
  fout << "# vtk DataFile Version 2.0" << std::endl;
  fout << "Conversion from CGNS file \"" << name << "\"" << std::endl;
  fout << "ASCII"  << std::endl;
}

// -------------------------------------------------------------
// avtk_coordinate
// -------------------------------------------------------------
void
avtk_coordinate(std::ostream& fout, CGNSFile& in)
{
  const CGNSFile::ZonePtr& zone = in.zone();
  int nv = zone->vertices();
  int nc = in.ncoords();
  std::vector<double> x(nv), y(nv), z(nv);
  double *vert;
  const char *name = "";
  for (int c = 1; c <= std::min<int>(nc, 3); c++) {
    switch (c) {
    case (1):
      name = "CoordinateX";
      vert = &x[0];
      break;
    case (2):
      name = "CoordinateY";
      vert = &y[0];
      break;
    case (3):
      name = "CoordinateZ";
      vert = &z[0];
      break;
    default:
      // this should not happen
      break;
    }
    in.coordread(name, vert);
  }
      
  fout << "POINTS " << zone->vertices() << " float" << std::endl;
      
  for (int i = 0; i < nv; i++) {
    fout << std::setiosflags(std::ios::scientific) 
         << std::setw(15) 
         << std::setprecision(6) 
         << x[i];
    fout << std::setiosflags(std::ios::scientific) 
         << std::setw(15) 
         << std::setprecision(6) 
         << y[i];
    fout << std::setiosflags(std::ios::scientific) 
         << std::setw(15) 
         << std::setprecision(6) 
         << z[i];
    fout << std::endl;
  }
}

// -------------------------------------------------------------
// avtk_structured
// -------------------------------------------------------------
void
avtk_structured(std::ostream& fout, CGNSFile& in)
{
  const CGNSFile::ZonePtr& zone = in.zone();
  fout << "DATASET STRUCTURED_GRID" << std::endl;
  fout << "DIMENSIONS " 
       << std::setw(5) << zone->size[0][0] << " " 
       << std::setw(5) << zone->size[0][1] << " " 
       << std::setw(5) << zone->size[0][2] << std::endl;
  avtk_coordinate(fout, in);
} 

// -------------------------------------------------------------
// avtk_unstructured
// -------------------------------------------------------------
void
avtk_unstructured(std::ostream& fout, CGNSFile& in)
{

  try {
    const CGNSFile::ZonePtr& zone = in.zone();
    fout << "DATASET UNSTRUCTURED_GRID" << std::endl;
    avtk_coordinate(fout, in);
    
    int ncells = zone->cells();
  
    int nsect = in.nsections();
    int ssize = 0;
    std::vector<int> celltype(ncells);
    int cidx = 0;

    for (int s = 1; s <= nsect; s++) {
      CGNSFile::SectionPtr sp(in.sectionread(s));
      if (sp->type != cgns::MIXED) {
        ssize += sp->size();
      }
      ssize += in.elementdatasize(sp);
    }

    fout << "CELLS " << ncells << " " << ssize << std::endl;
    for (int s = 1; s <= nsect; s++) {
      CGNSFile::SectionPtr sp(in.sectionread(s));
      cgns::ElementType_t intype;
      std::vector<cgns::cgsize_t> edata(in.elementdatasize(sp));
      in.elementsread(sp, &edata[0], NULL);
      cgns::cgsize_t *e = &edata[0];
      for (int c = 0; c < sp->size(); c++) {
        intype = sp->type;
        int np = in.npe(sp->type);
        if (sp->type == cgns::MIXED) {
          intype = (cgns::ElementType_t) *e++;
          np = in.npe(intype);
        }
        fout << std::setw(5) << np;
        for (int k = 0; k < np; k++) {
          fout << std::setw(8) << (*e++) - 1;
        }
        fout << std::endl;
      
        switch (intype) {
        case (cgns::TETRA_4): celltype[cidx++] = 10; break;
        case (cgns::PENTA_6): celltype[cidx++] = 13; break;
        case (cgns::HEXA_8): celltype[cidx++] = 12; break;
        default:
          throw std::runtime_error("unsupported cell type");
        }
      }
    }
    fout << "CELL_TYPES " << ncells << std::endl;
    for (int k = 0; k < ncells;) {
      for (int i = 0; i < 10 && k < ncells; i++, k++) {
        fout << std::setw(10) << celltype[k] << " " ;
      }
      fout << std::endl;
    } 
  } catch (const CGNSFile::error &e) {
    throw e;
  }

}

// -------------------------------------------------------------
// avtk_solution_scalar
// -------------------------------------------------------------
void
avtk_solution_scalar(std::ostream& fout, CGNSFile& in, const int& sidx,
                     const char *name)
{
  fout << "SCALARS " << name << " float" << " 1" << std::endl;
  fout << "LOOKUP_TABLE default" << std::endl;

  try {

    int n;

    CGNSFile::ZonePtr zone = in.zone();
    
    switch (in.solloc(sidx)) {
    case cgns::CellCenter:
      n = zone->cells();
      break;
    case cgns::Vertex:
      n = zone->vertices();
      break;
    default:
      throw CGNSFile::error("avtk_solution_scalar: unknown solution location");
    }
    std::vector<double> x(n);
    
    in.fieldread(sidx, name, &x[0]);

    for (int k = 0; k < n;) {
      for (int i = 0; i < 10 && k < n; i++, k++) {
        fout << std::setw(15) << std::scientific  << std::showpoint
             << std::setprecision(7) << x[k] << " ";
      }
      fout << std::endl;
    } 

  } catch (const CGNSFile::error &e) {
    throw e;
  }
}


// -------------------------------------------------------------
// avtk_solution_vector
// -------------------------------------------------------------
void
avtk_solution_vector(std::ostream& fout, CGNSFile& in, const int& sidx,
                     const char *name, 
                     const char *namex, const char *namey, const char *namez)
{
  fout << "VECTORS " << name << " float" << std::endl;
  try {

    int n;

    switch (in.solloc(sidx)) {
    case cgns::CellCenter:
      n = in.zone()->cells();
      break;
    case cgns::Vertex:
      n = in.zone()->vertices();
      break;
    default:
      throw CGNSFile::error("avtk_solution_vector: unknown solution location");
    }
    std::vector<double> x(n);
    std::vector<double> y(n);
    std::vector<double> z(n);

    in.fieldread(sidx, namex, &x[0]);
    in.fieldread(sidx, namey, &y[0]);
    in.fieldread(sidx, namez, &z[0]);

    for (int k = 0; k < n; k++) {
      fout << std::setw(15) << std::scientific  << std::showpoint
           << std::setprecision(7) << x[k] << " ";
      fout << std::setw(15) << std::scientific  << std::showpoint
           << std::setprecision(7) << y[k] << " ";
      fout << std::setw(15) << std::scientific  << std::showpoint
           << std::setprecision(7) << z[k] << " ";
      fout << std::endl;
    } 

  } catch (const CGNSFile::error &e) {
    throw e;
  }

  
}

// -------------------------------------------------------------
// avtk_solution
// -------------------------------------------------------------
void
avtk_solution(std::ostream& fout, CGNSFile& in, const int& sidx)
{
  std::list<std::string> fldnames;
  std::list<std::string>::const_iterator name;
  try {

    int n;
    
    n = in.nfields(sidx);
    for (int i = 0; i < n; i++) {
      std::string fname(in.fieldinfo(sidx, i+1));
      fldnames.push_back(fname);
    }
    
    switch (in.solloc(sidx)) {
    case cgns::CellCenter:
      fout << "CELL_DATA " << in.zone()->cells() << std::endl;
      break;
    case cgns::Vertex:
      fout << "POINT_DATA " << in.zone()->vertices() << std::endl;
      break;
    default:
      throw CGNSFile::error("avtk_solution_scalar: unknown solution location");
    }
    
    // Try to find vector names. The components should end with X, Y,
    // and Z

    std::set<std::string> vs;
    for (name = fldnames.begin(); name != fldnames.end(); name++) {
      std::string s(*name);
      if (boost::ends_with(s, "X")) {
        boost::erase_tail(s, 1);
        vs.insert(s);
      } else if (boost::ends_with(s, "Y")) {
        boost::erase_tail(s, 1);
        vs.insert(s);
      } else if (boost::ends_with(*name, "Z")) {
        boost::erase_tail(s, 1);
        vs.insert(s);
      }
    }

    // Make sure that each name has all three components, then remove
    // them from the scalar list

    if (!vs.empty()) {
      std::set<std::string>::const_iterator i;
      for (i = vs.begin(); i != vs.end(); i++) {
        std::string sx(*i); sx += "X";
        std::string sy(*i); sy += "Y";
        std::string sz(*i); sz += "Z";
        
        if ( (find(fldnames.begin(), fldnames.end(), sx) != fldnames.end()) &&
             (find(fldnames.begin(), fldnames.end(), sy) != fldnames.end()) &&
             (find(fldnames.begin(), fldnames.end(), sz) != fldnames.end()) ) {
          std::list<std::string>::iterator pos = fldnames.end();
          pos = remove(fldnames.begin(), pos, sx);
          pos = remove(fldnames.begin(), pos, sy);
          pos = remove(fldnames.begin(), pos, sz);
          fldnames.erase(pos, fldnames.end());
          avtk_solution_vector(fout, in, sidx, i->c_str(), 
                               sx.c_str(), sy.c_str(), sz.c_str());
        }
      }
    }

    for (name = fldnames.begin(); name != fldnames.end(); name++) {
      avtk_solution_scalar(fout, in, sidx, name->c_str());
    }
  } catch (const CGNSFile::error &e) {
    throw e;
  }
    
  
}

// -------------------------------------------------------------
//  Main Program
// -------------------------------------------------------------
int
main(int argc, char **argv)
{
  program = basename(argv[0]);

  // Parse command line options

  namespace po = boost::program_options;

  int opt;
  po::options_description desc("Available options");
  desc.add_options()
    ("help", "produce this help message")
    ("base", po::value<int>(&opt)->default_value(1), "CGNS base node index")
    ("zone", po::value<int>(&opt)->default_value(1), "zone index")
    ("solution", po::value<int>(&opt)->default_value(1), "solution index")
    ("cgns-file", po::value< std::string >(), "input CGNS file")
    ;

  po::positional_options_description p;
  p.add("cgns-file", 1);

  po::variables_map vm;
  po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
  po::notify(vm);    

  int base = vm["base"].as<int>();
  int zone = vm["zone"].as<int>();;
  int nsol = vm["solution"].as<int>();

  if (vm.count("help")) {
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << "\n";
    return 1;
  }

  if (vm.count("cgns-file") <= 0) {
    std::cerr << program << ": error: missing cgns-file" << std::endl;
    std::cerr << "Usage: " << program << " [options]" << std::endl;
    std::cerr << desc << "\n";
    return 1;
  }
  
  std::string inname(vm["cgns-file"].as<std::string>());

  // Open the file and read it in

  std::cerr << program << ": " << inname << ": " 
            << " extracting solution " << nsol
            << " from zone " << zone << " (base = " << base << ")" << std::endl;

  CGNSFile in;

  avtk_header(std::cout, inname.c_str());

  try {
    in.open(inname, CG_MODE_READ);
    in.base(base);
    in.zoneread(zone);

    int nc = in.ncoords();
    if (nc < 3) {
      std::cerr << argv[0] << ": error: " << inname << ": too few coordinates" << std::endl;
      exit(3);
    }

    switch (in.zone()->type) {
    case (cgns::Unstructured):
      avtk_unstructured(std::cout, in);
      break;
    case (cgns::Structured):
      avtk_structured(std::cout, in);
      break;
    default:
      std::cerr << argv[0] << ": error: " << inname 
                << ": unsupported grid type" << std::endl;
      break;
    }

    if (in.nsols() > 0) avtk_solution(std::cout, in, nsol);

  } catch (std::runtime_error &e) {
    std::cerr << argv[0] << ": error: " << e.what() << std::endl;
    exit(3);
  }

  return (0);

}

