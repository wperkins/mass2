#!/usr/bin/env python2
# -*- mode: python; py-which-shell: "python2";-*-
# -------------------------------------------------------------
# file: mkgrid.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 24, 2010 by William A. Perkins
# Last Change: Tue Mar 30 11:35:59 2010 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

import getopt, sys, os
from math import *

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])
usage = "usage: " + program

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
try:
    opts, args = getopt.getopt(sys.argv[1:], "h?")
except getopt.GetoptError:
    sys.stderr.write(usage + "\n")
    sys.exit(2)

for o, a in opts:
    if (o == "-h" or o == "-?"):
        sys.stderr.write(usage + "\n")
        sys.exit(0)

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

                                        # mesh block dimensions
h = 10.0
w = 100.0
l = 1000.0

                                        # numbers of cells in each block
nx = [ 100, 200 ]
ny = [  10,  30 ]

blocks = len(nx)

x = 0.0

for b in range(blocks):
    dx = l/float(nx[b])
    dy = w/float(ny[b])
    fname = ("grid%02d.dat" % (b))

    out = file(fname, "w")

    out.write("%d %d\n" % (nx[b]+1, ny[b]+1))
    
    for i in range(nx[b]+1):
        y = 0.0
        for j in range(ny[b]+1):
            z = 0.5*h*(cos(2.0*pi*y/w)+1)
            out.write("%5d %5d %8.2f %8.2f %8.2f / \n" %
                      (i, j, x, y, z))
            y += dy;
        x += dx;

    x -= dx;

    out.close()
    

