// -------------------------------------------------------------
// file: mass2flux.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created February 10, 2015 by William A. Perkins
// Last Change: 2015-02-12 07:11:09 d3g096
// -------------------------------------------------------------

#include <iostream>
#include <string>
#include <cmath>
#include <vector>
#include <boost/shared_ptr.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/program_options.hpp>
#include <boost/filesystem/path.hpp>

#include <blitz/blitz.h>
#include <blitz/array.h>

typedef blitz::Array<double, 2> CellField;
typedef blitz::Array<double, 2> NodeField;
typedef boost::shared_ptr<NodeField> NodeFieldPtr;
typedef boost::shared_ptr<CellField> CellFieldPtr;

typedef blitz::TinyVector<double, 2> Vector;
typedef blitz::Array<Vector, 2> CellVectorField;
typedef blitz::Array<Vector, 2> NodeVectorField;

#include "cgnsfile.h"

std::string program("unknown");

typedef std::vector<std::string> svector;

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
// new_node_field
// -------------------------------------------------------------
NodeField *
new_node_field(CGNSFile& in)
{
  NodeField *result;
  int inodes, jnodes;
  CGNSFile::ZonePtr z(in.zone());
  inodes = z->size[0][0];
  jnodes = z->size[0][1];
  result = new NodeField(jnodes, inodes);
  return result;
}

// -------------------------------------------------------------
// new_cell_field
// -------------------------------------------------------------
CellField *
new_cell_field(CGNSFile& in)
{
  CellField *result;
  int icells, jcells;
  CGNSFile::ZonePtr z(in.zone());
  icells = z->size[1][0];
  jcells = z->size[1][1];
  result = new CellField(jcells, icells);
  return result;
}

// -------------------------------------------------------------
// get_coordinate
// -------------------------------------------------------------
NodeVectorField *
get_coordinate(CGNSFile& in)
{
  boost::scoped_ptr<NodeField> xc(new_node_field(in));
  in.coordread(1, xc->data());
  boost::scoped_ptr<NodeField> yc(new_node_field(in));
  in.coordread(2, yc->data());
  NodeVectorField *result = new NodeVectorField(xc->shape());

  NodeField::iterator ix(xc->begin()), iy(yc->begin());
  NodeVectorField::iterator i(result->begin());

  for (; i != result->end(); ++i, ++ix, ++iy) {
    Vector v(*ix, *iy);
    *i = v;
  }
  
  return result;
}

// -------------------------------------------------------------
// create_cell_width field
// -------------------------------------------------------------
BZ_DECLARE_STENCIL2(cell_i_width, C, N)
  C = 0.5*(N(1,0) + N(1,1)) - (0.5*(N(0,0) + N(0,1)));
BZ_END_STENCIL

BZ_DECLARE_STENCIL2(cell_j_width, C, N)
  C = 0.5*(N(0,1) + N(1,1)) - (0.5*(N(0,0) + N(1,0)));
BZ_END_STENCIL

CellVectorField *
create_i_cell_width(CGNSFile& in)
{
  boost::scoped_ptr<NodeVectorField> c(get_coordinate(in));
  blitz::TinyVector<int, 2> shape(c->shape());
  shape -= 1;
  CellVectorField *result = new CellVectorField(shape);
  for (int i = 0; i < shape[1]; ++i) {
    for (int j = 0; j < shape[0]; ++j) {
      (*result)(j,i) = 
        0.5*((*c)(j+1,i) + (*c)(j+1,i+1)) -
        0.5*((*c)(j, i) + (*c)(j,i+1));
    }
  }
  // blitz::applyStencil(cell_i_width(), *result, *c);
  return result;
}

// -------------------------------------------------------------
// get_field
// -------------------------------------------------------------
CellField *
get_field(CGNSFile& in, const int& sidx, const std::string& fname)
{
  CellField *result = new_cell_field(in);
  in.fieldread(sidx, fname.c_str(), result->data());
  return result;
}

// -------------------------------------------------------------
// get_velocity
// -------------------------------------------------------------
CellVectorField *
get_velocity(CGNSFile& in, const int& sidx)
{
  boost::scoped_ptr<CellField> vx(get_field(in, sidx, "VelocityX"));
  boost::scoped_ptr<CellField> vy(get_field(in, sidx, "VelocityY"));
  CellVectorField *result = new CellVectorField(vx->shape());
  CellField::iterator ix(vx->begin()), iy(vy->begin());
  CellVectorField::iterator i(result->begin());
  for (; i != result->end(); ++i, ++ix, ++iy) {
    Vector v(*ix, *iy);
    *i = v;
  }
  return result;
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
  bool verbose;
  int thebase;
  int thezone;
  
  int istart(-1);
  int iend(-1);
  int jstart(-1);
  int jend(-1);

  std::string thefield;
  bool doskip(true);

  try {

    desc.add_options()
      ("help", "produce this help message")
      ("verbose", "produce some diagnostic messages")
      ("base", po::value<int>()->default_value(1), "CGNS base node index")
      ("zone", po::value<int>()->default_value(1), 
       "Index of zone whose solutions should be used")
      ("field", po::value< std::string >()->default_value(""), 
       "Solution field of interest -- average is computed by default")
      // ("average", "Compute a flux-weighted average of solution field")
      // ("flux", "Compute the flux of solution field (use --conversion, if needed)")
      // ("cumulative", "Compute the cumulative flux of the solution field (implies --flux)")
      // ("conversion", po::value<double>()->default_value(1), 
      //  "Factor to convert concentration field units to mass/ft^3")
      ("i-index", po::value<int>()->default_value(-1), 
       "Compute laterally at the specified index")
      ("no-skip", "do not skip (adjacent) indentically named solutions")
      ("cgns-input", po::value< svector >()->multitoken(), "input CGNS file")
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

    if (vm.count("cgns-input") <= 0) {
      std::cerr << program << ": error: missing cgns-input" << std::endl;
      std::cerr << "Usage: " << program << " [options]" << std::endl;
      std::cerr << desc << std::endl;
      return 1;
    }
  
    inname = vm["cgns-input"].as<svector>();
    verbose = (vm.count("verbose") > 0); 
    thebase = vm["base"].as<int>();
    thezone = vm["zone"].as<int>();

    istart = vm["i-index"].as<int>();
    iend = istart;

    thefield = vm["field"].as<std::string>();
    
    if (vm.count("no-skip") > 0) doskip = false;

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

  try {
    std::string lastname("bogus");
    for (svector::iterator in = inname.begin(); in != inname.end(); ++in) {

      if (verbose) {
        std::cerr << program << ": info: processing CGNS file " 
                  << "\"" << *in << "\""
                  << std::endl;
      }
      CGNSFile incgns(*in);
      incgns.base(thebase);
      incgns.zone(thezone);
      boost::scoped_ptr<CellVectorField> iw(create_i_cell_width(incgns));

      if (istart < 0) {
        istart = iw->extent(1)-1;
        iend = iw->extent(1)-1;
      }
      if (jstart < 0) {
        jstart = iw->base(0);
        jend = iw->extent(0)-1;
      }

      if (verbose) {
        std::cerr << program << ": info: computation region: "
                  << "(" << istart << ", " << jstart << ")" << ", "
                  << "(" << iend << ", " << jend << ")"
                  << std::endl;
      }

      int nsols(incgns.nsols());
      for (int isol = 1; isol <= nsols; ++isol) {
        std::string sname(incgns.solinfo(isol));
        if (doskip) {
          if (sname == lastname) {
            if (verbose) {
              std::cerr << program << ": info: " << *in << ": skipping duplicate solution " 
                        << isol << ", "<< "\"" << sname << "\""
                        << std::endl;
            }
            continue;
          }
        }
              
        if (verbose) {
          std::cerr << program << ": info: " << *in << ": processing solution " 
                    << isol << ", "<< "\"" << sname << "\""
                    << std::endl;
        }
          
        boost::scoped_ptr<CellVectorField> vel(get_velocity(incgns, isol));
        boost::scoped_ptr<CellField> depth(get_field(incgns, isol, "depth"));
        boost::scoped_ptr<CellField> field;
        if (!thefield.empty()) {
          field.reset(get_field(incgns, isol, thefield));
        }

        double vflux(0.0);
        double flux(0.0);
        for (int i = istart; i <= iend; ++i) {
          for (int j = jstart; j <= jend; ++j) {
            Vector iwidth((*iw)(j, i));
            Vector v((*vel)(j, i));
            double w(sqrt(dot(iwidth,iwidth)));
            Vector tmp(iwidth);
            tmp /= w;
            double vmag(dot(v, tmp));
            tmp *= vmag;
            v -= tmp;
            vmag = sqrt(dot(v, v));
            double ivflux = vmag*(*depth)(j, i)*w;
            double c(1.0);
            if (field) {
              c = (*field)(j, i);
            }
            double iflux = c*ivflux;
            vflux += ivflux;
            flux += iflux;
            
            // std::cout << i << ", " << j << ", " 
            //           << sname << ", "
            //           << (*vel)(j,i) << ", "
            //           << v << ", "
            //           << flux
            //           << std::endl;
        
          }
        }
        if (field) {
          flux = flux/vflux;
        }
        std::cout << sname << "  "
                  << flux << " / "
                  << std::endl;
        
      }
      incgns.close();
    }  
  } catch (const CGNSFile::error &e) {
    std::cerr << program << ": error: " << e.what()<< std::endl;
    exit(3);
  }
  
  return 0;
}

