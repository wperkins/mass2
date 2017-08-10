#!/usr/bin/env python
# -*- mode: python -*-
# -------------------------------------------------------------
# file: profile.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 21, 2012 by William A. Perkins
# Last Change: 2015-04-30 09:59:46 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os
from optparse import OptionParser
import CGNS
import numarray
from math import *

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])
usage = "usage: " + program

usage = "Usage: %prog [options] plot.cgns field"
parser = OptionParser(usage=usage)


parser.add_option("-v", "--verbose",
                  dest="verbose", action="store_true", default=False,
                  help="show whats going on")

parser.add_option("-B", "--base", type="int", default=1,
                  dest="base", action="store",
                  metavar="index",
                  help="The base index")

parser.add_option("-b", "--block", type="int", default=1,
                  dest="block", action="store",
                  metavar="index",
                  help="The block index")

parser.add_option("-s", "--solution", type="int", default=-1,
                  dest="solution", action="store",
                  metavar="index",
                  help="The solution index (<0 means last)")

parser.add_option("-I", "--i-index", type="int", default=-1,
                  dest="iindex", action="store",
                  metavar="i",
                  help="The i index for which to extract the profile")

parser.add_option("-J", "--j-index", type="int", default=-1,
                  dest="jindex", action="store",
                  metavar="j",
                  help="The j index for which to extract the profile")

parser.add_option("-o", "--output", type="string",
                  dest="output", metavar="file", action="store")

(options, args) = parser.parse_args()

if (len(args) < 2):
    parser.print_help()
    sys.exit(3)

doverbose = options.verbose
thebase = options.base
theblock = options.block
thesol = options.solution
theiidx = options.iindex
thejidx = options.jindex

if (theiidx < 0 and thejidx < 0):
    sys.stderr.write("%s: error: either --i-index or --j-index must be specified\n" %
                     (program));
    parser.print_help()
    sys.exit(3)

if (theiidx > 0 and thejidx > 0):
    sys.stderr.write("%s: error: cannot specify both --i-index or --j-index\n" %
                     (program));
    parser.print_help()
    sys.exit(3)
    
    
pfile = args[0]
thefield = args[1]

if options.output:
    output = open(options.output, mode='w')
else:
    output = sys.stdout


# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

if doverbose:
    sys.stderr.write("%s: info: processing base %d, zone %d of file %s\n" %
                     (program, thebase, theblock, pfile));

cgns = CGNS.pyCGNS(pfile, CGNS.MODE_READ)

zinfo = cgns.zoneread(thebase, theblock)
#print zinfo
imin = 1
jmin = 1
imax = zinfo[3][2]
jmax = zinfo[3][3]
ivmax = zinfo[3][0]
jvmax = zinfo[3][1]

if doverbose:
    sys.stderr.write("%s: info: zone %d size = %d x %d\n" %
                     (pfile, theblock, imax, jmax))

if theiidx > 0:
    imin = theiidx
    imax = theiidx

if thejidx > 0:
    jmin = thejidx
    jmax = thejidx

nsols = cgns.nsols(thebase, theblock)

if thesol > 0:
    sidx = thesol
else:
    sidx = nsols

x = cgns.coordread(thebase, theblock, CGNS.CoordinateX)
x = numarray.reshape(x, (jvmax, ivmax))
y = cgns.coordread(thebase, theblock, CGNS.CoordinateY)
y = numarray.reshape(y, (jvmax,ivmax))
#print x.shape


if (thefield == "zbot"):
    z = cgns.fieldread(thebase, theblock, sidx, "wsel",
                       CGNS.RealDouble,
                       (imin, jmin,0),
                       (imax, jmax, 0))
    
    tmp = cgns.fieldread(thebase, theblock, sidx, "depth",
                         CGNS.RealDouble,
                         (imin, jmin,0),
                         (imax, jmax, 0))

    tmp = z - tmp
else:
    tmp = cgns.fieldread(thebase, theblock, sidx, thefield,
                         CGNS.RealDouble,
                         (imin, jmin,0),
                         (imax, jmax, 0))

if (theiidx > 0):
    z = numarray.reshape(tmp, (jmax - jmin + 1) )
elif (thejidx > 0):
    z = numarray.reshape(tmp, (imax - imin + 1) )

#print z.shape

cgns.close()

dc = 0
first = True
lastxc = 0
lastyc = 0
idx = 0
for i in range(imin, imax+1):
    for j in range(jmin, jmax+1):
        #print i, j
        xc = (x[j-1,i-1] + x[j-1,i] + x[j,i] + x[j,i-1])*0.25
        yc = (y[j-1,i-1] + y[j-1,i] + y[j,i] + y[j,i-1])*0.25
        if not first:
            dx = xc - lastxc
            dy = yc - lastyc
            dc += sqrt(dx*dx + dy*dy)
        else:
            dc = 0.0
        output.write("%8d %8d %13.3f %13.3f %11.4e %11.4e\n" % 
                     (i, j, xc, yc, dc, z[idx]))
        lastxc = xc
        lastyc = yc
        first = False
        idx = idx + 1


output.close()
