// Emacs Mode Line: -*- Mode:c++;-*-
// -------------------------------------------------------------
// file: DateTime.h
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created November 28, 2018 by William A. Perkins
// Last Change: 2018-11-28 13:11:17 d3g096
// -------------------------------------------------------------


#ifndef _DateTime_h_
#define _DateTime_h_

#include <string>
#include <boost/date_time/posix_time/posix_time.hpp>

typedef boost::gregorian::date Date;
typedef boost::posix_time::ptime DateTime;
typedef boost::posix_time::time_duration Duration;

extern const char mass2_datetime_fmt[];

DateTime strp_datetime(const char* s, const char* fmt);
std::string strf_datetime(const DateTime d, const char* fmt);

#endif
