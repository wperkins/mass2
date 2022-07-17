// -------------------------------------------------------------
// file: DateTime.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created November 28, 2018 by William A. Perkins
// Last Change: 2018-11-29 11:19:18 d3g096
// -------------------------------------------------------------

#include <boost/format.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/date_time/local_time/local_time.hpp>
#include <sstream>
#include "DateTime.h"

const char mass2_datetime_fmt[] = "%m-%d-%Y %H:%M:%S";

// -------------------------------------------------------------
// strp_datetime
// -------------------------------------------------------------
DateTime
strp_datetime(const char* s, const char* fmt)
{
  struct tm pt_tm;

  pt_tm.tm_sec = 0;
  pt_tm.tm_min = 0;
  pt_tm.tm_hour = 0;
  pt_tm.tm_mday = 0;
  pt_tm.tm_mon = 0;
  pt_tm.tm_year = 0;
  pt_tm.tm_wday = 0;
  pt_tm.tm_yday = 0;
  pt_tm.tm_isdst = 0;
  pt_tm.tm_gmtoff = 0;
  pt_tm.tm_zone = NULL;

  if (strptime(s, fmt, &pt_tm) == NULL) {
    std::string msg = 
      boost::str(boost::format("%s: unable to parse date") % s);
    throw std::runtime_error(msg);
  }

  DateTime result(boost::posix_time::ptime_from_tm(pt_tm));
  return result;
}


// -------------------------------------------------------------
// strf_datetime
// -------------------------------------------------------------
std::string
strf_datetime(const DateTime d, const char* fmt)
{
  std::string result;

  boost::posix_time::time_facet *output_facet =
    new boost::posix_time::time_facet(fmt);
  std::stringstream ss;
  // ss.imbue(std::locale(std::locale::classic(), output_facet));

  ss << d;

  result = ss.str();

  return result;
}
