// -------------------------------------------------------------
// file: cgnssplit.cpp

/**
 * @file   cgnssplit.cpp
 * @author William A. Perkins
 * @date Thu Jun 28 10:07:04 2012
 * 
 * @brief A program to split a single zone into multiple zones based
 * on a solution field.
 * 
 * 
 */

// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created May 19, 2007 by William A. Perkins
// Last Change: Thu Jun 28 10:07:04 2012 by William A. Perkins <d3g096@pe10900.pnl.gov>
// -------------------------------------------------------------


static const char* SCCS_ID = "$Id$ Battelle PNL";

#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <string>
#include <algorithm>
#include <boost/program_options.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/format.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/casts.hpp>
#include <boost/spirit/include/classic.hpp>

#include "cgnsfile.h"

/// A place to remember the name of this program
std::string program;

/// A basic integer array
typedef std::vector<cgns::cgsize_t> ivector;

/// A basic double array
typedef std::vector<double> dvector;

/// The number of cells in the original zone
int ncells;

/// The number of vertexes in the original zone
int nverts;

/// A place to store the cell type   
ivector celltype(ncells);

/// A place to store the vertex indexes by cell
std::vector< ivector > cellvtx;

/// How vertexes are assigned to out blocks
std::vector < ivector > blockvtx;

/// How vertexes are numbered in out blocks
std::vector < ivector > blockoutvtx;  

/// The default name of the DiscreteData/FlowSolution node in which decomposition data is stored
static const char *default_discrete_name = "Decomposition";

/// The default name of the field containing the decomposition index
static const char *default_field_name = "Part";

/// Name of discrete data node for "old" mesh info
static std::string old_cell_data_name("OriginalCell");

/// Name of  array of "old" mesh cell indexes
static std::string old_cell_index_name("OriginalCellIndex");

/// Name of discrete data node for "old" mesh info
static std::string old_vertex_data_name("OriginalVertex");

/// Name of  array of "old" mesh cell indexes
static std::string old_vertex_index_name("OriginalVertexIndex");

/// A place to remember to which zones (existing) block connections were written
static std::map<std::string, std::string> connection_zone;

/// Minimum number of vertexes that a boundary/connection can have
static unsigned int minimum_boundary_vertexes = 3;

// -------------------------------------------------------------
// read_topology
// -------------------------------------------------------------

/** 
 * Reads the current zone from @c in and populates the global topology
 * data structures
 * 
 * @param in 
 */
void
read_topology(CGNSFile& in)
{
  ncells = in.zone()->cells();
  nverts = in.zone()->vertices();
  celltype.reserve(ncells);
  cellvtx.reserve(ncells);

  int nsect = in.nsections();

  for (int s = 1; s <= nsect; s++) {
    CGNSFile::SectionPtr sp(in.sectionread(s));
    ivector edata(in.elementdatasize(sp));

    in.elementsread(sp, &edata[0], NULL);
    int eidx = 0;
    for (int c = sp->idxbeg; c <= sp->idxend; c++) {
      switch (sp->type) {
      case cgns::MIXED: 
        celltype[c-1] = edata[eidx++];
        break;
      default:
        celltype[c-1] = static_cast<int>(sp->type);
        break;
      }
      for (int i = 0; 
           i < in.npe(static_cast<cgns::ElementType_t>(celltype[c-1])); 
           i++) {
        cellvtx[c-1].push_back(edata[eidx++]);
      }
    }
  }
}
  
// -------------------------------------------------------------
// get_unique_values
// -------------------------------------------------------------
const ivector 
get_unique_values(const ivector& values)
{
  ivector unique_values;
  ivector tmp(values);
  std::sort(tmp.begin(), tmp.end());
  std::unique_copy(tmp.begin(), tmp.end(), 
                   std::back_inserter(unique_values));
  return unique_values;
}
  

// -------------------------------------------------------------
// extract_partition
// -------------------------------------------------------------
/** 
 * Extract the specified cells from the current zone in @c in.  
 * 
 * @param in input CGNS file
 * @param cellidx zero-based cell indexes
 * @param out output CGNSFile
 * @param outzonename base name for output zones
 * @param dosol split solution fields too?
 */
void
extract_partition(CGNSFile& in, ivector& cellidx, 
                  CGNSFile& out, const std::string& outzonename,
                  const bool dosol = false)
{

  // Make a unique list of vertex indexes and build element data for
  // output section

  ivector vtxidx;
  vtxidx.reserve(nverts);
  for (ivector::const_iterator c = cellidx.begin();
       c != cellidx.end(); c++) {
    std::copy(cellvtx[*c].begin(), cellvtx[*c].end(), 
              std::back_inserter(vtxidx));
  }

  // map old vertex indexes to new

  ivector outvtx(get_unique_values(vtxidx));

  blockvtx.push_back(outvtx);
  
  std::cerr << (boost::format("%1%: info: writing %2% cells and %4% vertexes to zone \"%3%\"") 
                % program % cellidx.size() % outzonename % outvtx.size())
            << std::endl;

  ivector vtxmap(nverts+1, 0);
  for (unsigned iv = 0; iv < outvtx.size(); iv++) {
    vtxmap[outvtx[iv]] = iv+1;
  }

  blockoutvtx.push_back(vtxmap);

  cgns::cgsize_t size[3] = { outvtx.size(), cellidx.size(), 0 };
  CGNSFile::ZonePtr outzone 
    = out.zonewrite(outzonename.c_str(), size, cgns::Unstructured);

  ivector outedata;
  outedata.reserve(cellidx.size()*9);
  for (ivector::const_iterator c = cellidx.begin();
       c != cellidx.end(); c++) {
    outedata.push_back(celltype[*c]);
    for (ivector::const_iterator iv = cellvtx[*c].begin();
         iv != cellvtx[*c].end(); iv++) {
      int outid = vtxmap[*iv];
      outedata.push_back(outid);
    }
  }
  // transfer vertex coordinates

  int ncoords(in.ncoords());
  dvector tmp(nverts);
  dvector outcoord(outzone->vertices());
  for (int ic = 1; ic <= ncoords; ic++) {
    CGNSFile::ArrayPtr cinfo = in.coordinfo(ic);
    in.coordread(ic, &tmp[0]);
    for (unsigned int iv = 0; iv < outvtx.size(); iv++) {
      outcoord[iv] = tmp[outvtx[iv] - 1];
    }
    out.coordwrite(cinfo->name.c_str(), &outcoord[0]);
  }

  // write the cell/vertex topology

  CGNSFile::SectionPtr outsp = out.newsection();
  outsp->name = "CellData";
  outsp->type = cgns::MIXED;
  outsp->idxbeg = 1;
  outsp->idxend = cellidx.size();
  out.sectionwrite(outsp, &outedata[0]);

  // split boundaries too

  int inbc = in.nbc();
  for (int bc = 1; bc <= inbc; bc++) {
    CGNSFile::BCPtr bcinfo = in.bcinfo(bc);
    int npts = bcinfo->npts;
    ivector inbcpts(npts);
    ivector outbcpts;
    outbcpts.reserve(npts);
    in.bcread(bc, &inbcpts[0]);
    for (unsigned int iv = 0; iv < inbcpts.size(); iv++) {
      if (vtxmap[inbcpts[iv]] > 0) {
        outbcpts.push_back(vtxmap[inbcpts[iv]]);
      }
    }
    if (outbcpts.size() >= minimum_boundary_vertexes) {
      std::cerr << (boost::format("%1%: info: writing %2% (of %3%) vertexes for boundary \"%4%\" in zone \"%5%\"") 
                    % program % outbcpts.size() % npts % bcinfo->name % outzonename)
                << std::endl;
      CGNSFile::BCPtr outbc = out.newbc();
      outbc->type = bcinfo->type;
      outbc->name = bcinfo->name;
      outbc->npts = outbcpts.size();
      out.bcwrite(outbc, &outbcpts[0]);
    }
  }

  // split all connections too FIXME: should identify the connecting
  // zone; should not allow a connection to be split

  int inconn = in.nconns();
  for (int iconn = 1; iconn <= inconn; iconn++) {
    CGNSFile::ConnPtr conninfo = in.conninfo(iconn);
    unsigned int npts = conninfo->npnts;
    ivector inbcpts(npts);
    ivector outbcpts;
    outbcpts.reserve(npts);
    in.connread(iconn, &inbcpts[0], NULL, NULL);
    for (unsigned int iv = 0; iv < inbcpts.size(); iv++) {
      if (vtxmap[inbcpts[iv]] > 0) {
        outbcpts.push_back(vtxmap[inbcpts[iv]]);
      }
    }
    if (outbcpts.size() == npts) {
      std::cerr << (boost::format("%1%: info: writing %2% (of %3%) vertexes for connection \"%4%\" in zone \"%5%\"") 
                    % program % outbcpts.size() % npts % conninfo->name % outzonename)
                << std::endl;
      CGNSFile::ConnPtr outconn(out.newconn());
      outconn->name = conninfo->name;
      outconn->conntype = conninfo->conntype;
      outconn->location = conninfo->location;
      outconn->donorname = outzonename; // to be set later -- needs to be a valid zone name!
      outconn->donorconnname = conninfo->donorconnname;
      outconn->ptsettype = conninfo->ptsettype;
      outconn->npnts = outbcpts.size();
      outconn->ndata_donor = 0;
      outconn->periodic = conninfo->periodic;

      if (conninfo->periodic) {
        for (int i = 0; i < 3; i++) {
          outconn->periodic_rcenter[i] = conninfo->periodic_rcenter[i];
          outconn->periodic_rotation[i] = conninfo->periodic_rotation[i];
          outconn->periodic_tranlation[i] = conninfo->periodic_tranlation[i];
        }
      }

      out.connwrite(outconn, &outbcpts[0], NULL, NULL);

      // remember the zone this connection belongs to
      connection_zone.insert(std::pair<std::string, std::string>(outconn->name, outconn->zone->name));

    } else if (outbcpts.size() > 0) {
      std::cerr << boost::str(boost::format("%1%: error: will not write partial connection (%2% of %3% vertexes) for connection \"%4%\" in zone \"%5%\"") 
                              % program % outbcpts.size() % npts % conninfo->name % outzonename)
                << std::endl;
    }

  }

  // split all solution fields (if called for) 
  // Requires cell-centered solutions

  if (dosol) {
    dvector inval(in.zone()->cells());
    dvector outval(cellidx.size());
    int nsols = in.nsols();
    for (int isol = 1; isol <= nsols; isol++) {
      std::string sname(in.solinfo(isol));
      int osol = out.solwrite(sname.c_str(), cgns::CellCenter);
      int nflds = in.nfields(isol);
      for (int ifld = 1; ifld <= nflds; ifld++) {
        std::string fldname(in.fieldinfo(isol, ifld));
        in.fieldread(isol, fldname.c_str(), &inval[0]);
        outval.clear();
        for (ivector::const_iterator c = cellidx.begin();
             c != cellidx.end(); c++) {
          outval.push_back(inval[*c]);
        }
        out.fieldwrite(osol, fldname.c_str(), &outval[0]);
      }
    }
  }

  // add a discrete data field that contains the old cell indexes (1-based)

  int didx = out.discretewrite(old_cell_data_name.c_str(), cgns::CellCenter);
  out.discretefieldwrite(didx, old_cell_index_name.c_str(), cgns::CellCenter, &cellidx[0]);

  didx = out.discretewrite(old_vertex_data_name.c_str(), cgns::Vertex);
  out.discretefieldwrite(didx, old_vertex_index_name.c_str(), cgns::Vertex, &outvtx[0]);
}

// -------------------------------------------------------------
// build_block_connections
// -------------------------------------------------------------
void
build_block_connections(CGNSFile& cgnsout)
{
  for (unsigned int blk1 = 0; blk1 < blockvtx.size(); blk1++) {
    for (unsigned int blk2 = blk1+1; blk2 < blockvtx.size(); blk2++) {
      ivector vconn;
      std::set_intersection(blockvtx[blk1].begin(), blockvtx[blk1].end(),
                            blockvtx[blk2].begin(), blockvtx[blk2].end(),
                            std::back_inserter(vconn));

      if (vconn.size() < minimum_boundary_vertexes) continue;

      CGNSFile::ZonePtr lzone = cgnsout.zone(blk1+1);
      CGNSFile::ConnPtr lconn = cgnsout.newconn();
      CGNSFile::ZonePtr rzone = cgnsout.zone(blk2+1);
      CGNSFile::ConnPtr rconn = cgnsout.newconn();

      std::cerr << (boost::format("%1%: info: connecting block %2% to block %3% with %4% vertexes") 
                    % program % lzone->name % rzone->name % vconn.size())
                << std::endl;

      lconn->name = 
        boost::str(boost::format("%1%_to_%2%") % lzone->name % rzone->name);
      rconn->name = 
        boost::str(boost::format("%2%_to_%1%") % lzone->name % rzone->name);

      ivector lvout(vconn.size()), rvout(vconn.size());

      for (unsigned int iv = 0; iv < vconn.size(); iv++) {
        lvout[iv] = blockoutvtx[blk1][vconn[iv]];
        rvout[iv] = blockoutvtx[blk2][vconn[iv]];
      }

      lconn->conntype = cgns::Abutting1to1;
      lconn->location = cgns::Vertex;
      lconn->donorname = rzone->name;
      lconn->donorconnname = rconn->name;
      lconn->donor_zonetype = rzone->type;
      lconn->ptsettype = cgns::PointList;
      lconn->npnts = vconn.size();
      lconn->donor_ptset_type = cgns::PointListDonor;
      lconn->ndata_donor = lconn->npnts;
    
      cgnsout.connwrite(lconn, &lvout[0], &rvout[0], NULL);
      
      rconn->conntype = cgns::Abutting1to1;
      rconn->location = cgns::Vertex;
      rconn->donorname = lzone->name;
      rconn->donorconnname = lconn->name;
      rconn->donor_zonetype = lzone->type;
      rconn->ptsettype = cgns::PointList;
      rconn->npnts = vconn.size();
      rconn->donor_ptset_type = cgns::PointListDonor;
      rconn->ndata_donor = rconn->npnts;
      
      cgnsout.connwrite(rconn, &rvout[0], &lvout[0], NULL);
    }
  }
}
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
// get_split
// -------------------------------------------------------------
void
get_split(CGNSFile& in, const std::string& nodename, 
          const std::string& fname, ivector& split)
{
  std::string msg;

  // Reading from current zone ...

  int ncells = in.zone()->cells();

  // Try to find the discrete data node first
  
  if (in.ndiscrete() > 0) {
    int didx;
    cgns::GridLocation_t loc;
    bool ok = true;
    try {
      in.discreteread(nodename.c_str(), didx, loc);
    } catch (CGNSFile::error& e) {
      ok = false;
    }
    if (ok) {
      if (loc != cgns::CellCenter) {
        msg = "discrete data node \"";
        msg += nodename;
        msg += "\" is not cell-centered";
        throw CGNSFile::error(msg);
      }
      in.discretefieldread(didx, fname.c_str(), &split[0]);
      return;
    }
  }
  
  if (in.nsols() > 0) {
    for (int s = 1; s <= in.nsols(); s++) {
      if (in.solinfo(s) == nodename) {
        dvector dtmp(ncells);
        in.fieldread(s, fname.c_str(), &dtmp[0]);
        std::transform(dtmp.begin(), dtmp.end(), split.begin(),
                       boost::lambda::ll_static_cast<int>(boost::lambda::_1));
        return;
      }
    }
  }

  msg = in.zone()->name + ": no decomposition data found";
  throw CGNSFile::error(msg);
}


// -------------------------------------------------------------
// reconnect_old_connections
// -------------------------------------------------------------
void
reconnect_old_connections(CGNSFile& out)
{
  
  int nzones = out.zones();
  for (int z = 1; z <= nzones; z++) {
    out.zone(z);
    int nconns = out.nconns();
    for (int c = 1; c <= nconns; c++) {
      CGNSFile::ConnPtr conn = out.conninfo(c);
      std::map<std::string, std::string>::const_iterator d;

      d = connection_zone.find(conn->donorconnname);

      if (d != connection_zone.end()) {
        conn->donorname = d->second;
  
        std::cerr << (boost::format("%1%: info: reconnecting %2%: %3% to %4%:%5%") 
                % program % conn->zone->name % conn->name % conn->donorname % conn->donorconnname)
            << std::endl;
        
        ivector bcpts(conn->npnts);
        out.connread(c, &bcpts[0], NULL, NULL);
        out.connwrite(conn, &bcpts[0], NULL, NULL);
      }
    }
  }
}

// -------------------------------------------------------------
// read_scotch_split
// -------------------------------------------------------------
int
read_scotch_split(const std::string& fname, ivector& split)
{
  std::ifstream infile(fname.c_str());

  int ierr(0);
  int tmp, icell, part;
  char cbuf[1024];

  using namespace boost::spirit::classic;

  rule <phrase_scanner_t> empty = ( end_p );

  rule <phrase_scanner_t> rule1 = 

                                // just one integer
    int_p[assign_a(tmp)] >>
                                // there may be stuff at the end
    ( (*(anychar_p) >> end_p)  || end_p );


  rule <phrase_scanner_t> rule2 = 

                                // cell id
    int_p[assign_a(icell)] >>
                                // partition id
    int_p[assign_a(part)] >>
                                // skip stuff at the end
    ( (*(anychar_p) >> end_p)  || end_p );

  parse_info<const char *> presult;
  int lnum = 0;

  // The first line is the number of cells, check to make sure it's
  // right

  infile.getline(cbuf, sizeof(cbuf));
  lnum++;

  presult = parse(cbuf, rule1, space_p);

  if (!presult.full) {
    std::cerr << program << ": " << fname << ", " << lnum 
              << ": error: parse error"
              << std::endl;
    return 1;
  }

  if (tmp != split.size()) {
    std::cerr << program << ": " << fname << ", " << lnum 
              << ": error: incorrect cell count "
              << "(got " << tmp << ", expected " << split.size() << ")"
              << std::endl;
    return 1;
  }

  // The rest of the file is mapping
  
  while (infile) {
    infile.getline(cbuf, sizeof(cbuf));
    lnum++;

    // skip empty lines
    presult = parse(cbuf, empty, space_p);
    if (presult.full) continue;

    presult = parse(cbuf, rule2, space_p);
    if (!presult.full) {
      std::cerr << program << ": " << fname << ", " << lnum 
                << ": error: parse error"
                << std::endl;
      return 1;
    }
    
    split[icell] = part;

  }

  infile.close();
  return ierr;
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
    ("base", po::value<int>()->default_value(1), "CGNS base node index (in input or like)")
    ("zone", po::value<int>()->default_value(1), "zone index (in input or like)")
    ("discrete-name", po::value< std::string >()->default_value(default_discrete_name),
     "solution/discrete data node name (in input or like)")
    ("field-name", po::value< std::string >()->default_value(default_field_name), 
     "solution field used for split (converted to integer)")
    ("split-solution", "split all flow solution too")
    ("vertex-minimum", po::value<unsigned int>()->default_value(minimum_boundary_vertexes),
     "minimum number of vertexes to comprise a boundary")
    ("scotch-map", po::value< std::string >()->default_value(""), "read partition from a Scotch map file")
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
  int zone = vm["zone"].as<int>();
  bool dosol = vm.count("split-solution") > 0;

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
  std::string discretename(vm["discrete-name"].as<std::string>());
  std::string fieldname(vm["field-name"].as<std::string>());
  minimum_boundary_vertexes = vm["vertex-minimum"].as<unsigned int>();

  std::string scotchname(vm["scotch-map"].as<std::string>());
  bool doscotch = (scotchname.size() > 0);

  CGNSFile in, out;
  
  try {
    in.open(inname.c_str(), CG_MODE_READ);
    out.open(outname.c_str(), CG_MODE_WRITE);

    CGNSFile::BasePtr inbase = in.baseread(base);
    CGNSFile::ZonePtr z = in.zoneread(zone);
    
    switch (in.zone()->type) {
    case (cgns::Unstructured):
      break;
    case (cgns::Structured):
    default:
      std::cerr << argv[0] << ": error: " << inname 
                << ": only Unstructured zones supported" << std::endl;
      exit(3);
      break;
    }

    read_topology(in);

    // Get the split field and store it as an integer
    ivector split(ncells);

    if (doscotch) {
      if (read_scotch_split(scotchname, split)) {
        std::cerr << program << ": error: " 
                  << scotchname << " cannot read"
                  << std::endl;
        exit(3);
      }
    } else {
      get_split(in, discretename, fieldname, split);
    }

    // Find the unique values of the split field
    ivector splitvalues(get_unique_values(split));
    std::cerr << program << ": info: splitting values: ";
    std::copy(splitvalues.begin(), splitvalues.end(), 
              std::ostream_iterator<int>(std::cerr, ","));
    std::cerr << std::endl;

    CGNSFile::BasePtr outbase = out.basewrite(inbase->name.c_str(), 
                                              inbase->celldim, inbase->physdim);


    // Split the zone into several zones and put in output; this takes
    // care of boundary conditions too

    for (ivector::const_iterator isplit = splitvalues.begin();
         isplit != splitvalues.end(); isplit++) {
      ivector cellidx;
      cellidx.reserve(ncells);
      for (int c = 0; c < ncells; c++) {
        if (split[c] == *isplit) cellidx.push_back(c);
      }
      std::string outzname;
      outzname = boost::str(boost::format("%1%%2$03d") % fieldname % *isplit);
      extract_partition(in, cellidx, out, outzname, dosol);
    }

    // build block connections with shared vertexes

    build_block_connections(out);

    in.close();
    out.close();

    out.open(outname.c_str(), CG_MODE_MODIFY);

    out.base(base);

    reconnect_old_connections(out);

    out.close();
  } catch (std::runtime_error &e) {
    std::cerr << argv[0] << ": error: " << e.what() << std::endl;
    exit(3);
  }
    
}


