#!/usr/bin/env python
# -*- python -*-
# -------------------------------------------------------------
# file: mkgrid.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 27, 2002 by William A. Perkins
# Last Change: Tue Jan 29 12:38:55 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

# import math
import sys

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
width = 100.0 # 40.0
height = 5.0
len = 1000.0
bwidth = 44.0 # 10.0

v = 5.0
depth = height - 1.0
n = 0.026

dx = 20.0
dy = 4.0

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

                                        # compute the normal flow

swidth = (width - bwidth)/2
sslope = height/swidth

A = 0.5*(bwidth + width)*depth
R = bwidth + 2.0*pow(pow(depth/sslope, 2.0) + pow(depth,2.0), 0.5)
R = A/R
Q = v*A
slope = pow(n*v/(1.49*pow(R, 2./3.)), 2.0)

print 'R = %.1f ft, A = %.2f ft, Q = %.1f cfs' % (R, A, Q)
print 'slope = %.5g' % slope

nx = int(len/dx)
ny = int(width/dy)

print '%5d %5d' % (nx + 1, ny + 1)

for i in range(nx + 1):
    x = i*dx
    bottom = (nx - i)*dx*slope
    for j in range(ny+1):
        y = j*dy
        if y < swidth:
            z = height - sslope*y + bottom
        elif (y > swidth + bwidth):
            z = (y - bwidth - swidth)*sslope + bottom
        else:
            z = bottom
            
        print '%5d %5d %13.6f %13.6f %13.6f' % (i, j, x, y, z)

