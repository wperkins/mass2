#!/usr/bin/env python2
# -*- mode: python; py-which-shell: "python2";-*-
# -------------------------------------------------------------
# file: profile.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 21, 2012 by William A. Perkins
# Last Change: Thu Jun  3 06:45:08 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
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

usage = "Usage: %prog [options] plot000.cgns"
parser = OptionParser(usage=usage)

parser.add_option("-v", "--verbose",
                  dest="verbose", action="store_true", default=False,
                  help="show whats going on")

parser.add_option("-o", "--output", type="string",
                  dest="output", metavar="file", action="store")

(options, args) = parser.parse_args()

doverbose = options.verbose

if (len(args) < 1):
    parser.print_help()
    sys.exit(3)
    
pfile = args[0]

if options.output:
    output = open(options.output, mode='w')
else:
    output = sys.stdout


# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

b = 1
z = 1

if doverbose:
    sys.stderr.write("%s: info: processing base %d, zone %d of file %s\n" %
                     (program, b, z, pfile));

cgns = CGNS.pyCGNS(pfile, CGNS.MODE_READ)

zinfo = cgns.zoneread(b, z)
# print zinfo
imax = zinfo[3][0]
jmax = zinfo[3][1]
nsols = cgns.nsols(b, z)

if doverbose:
    sys.stderr.write("%s: info: zone %d size = %d x %d\n" %
                     (pfile, z, imax, jmax))

x = cgns.coordread(b, z, CGNS.CoordinateX)
x = numarray.reshape(x, (jmax,imax))
y = cgns.coordread(b, z, CGNS.CoordinateY)
y = numarray.reshape(y, (jmax,imax))
# print x.shape

tmp = cgns.fieldread(b, z, nsols, "depth",
                     CGNS.RealDouble,
                     (1,1,0),
                     (imax, 1, 0))

depthrb = numarray.reshape(tmp, (imax) )

tmp = cgns.fieldread(b, z, nsols, "depth",
                     CGNS.RealDouble,
                     (1,jmax,0),
                     (imax, jmax, 0))
depthlb = numarray.reshape(tmp, (imax) )

cgns.close()

dc = 0
jc = int(jmax/2)-1
for i in range(imax):
    if (i > 0):
        dx = x[jc,i-1] - x[jc,i]
        dy = y[jc,i-1] - y[jc,i]
        dc += sqrt(dx*dx + dy*dy)
    output.write("%d %.4e %.4e %.4e\n" %
                 (i, dc, depthlb[i], depthrb[i]))


output.close()
