// Emacs Mode Line: -*- Mode:c++;-*-
// -------------------------------------------------------------
// file: mybasename.h
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created October 25, 2017 by William A. Perkins
// Last Change: 2017-10-25 07:34:29 d3g096
// -------------------------------------------------------------


#ifndef _mybasename_h_
#define _mybasename_h_

#include <string>
#include <boost/filesystem/path.hpp>

// -------------------------------------------------------------
// basename
// -------------------------------------------------------------
inline const std::string
mybasename(const char* p)
{
  boost::filesystem::path progpath(p);
  std::string bname(progpath.filename().string());
  return bname;
}


#endif
