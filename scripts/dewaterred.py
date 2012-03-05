#!/usr/bin/env python
# -*- mode: python; -*-
# -------------------------------------------------------------
# file: dewaterred.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 30, 2011 by William A. Perkins
# Last Change: Wed Feb 29 14:01:44 2012 by William A. Perkins <d3g096@flophouse>
# -------------------------------------------------------------

# RCS ID: $Id: dewaterred.py,v 1.6 2011/10/25 17:56:04 d3g096 Exp $

import sys, os
from optparse import OptionParser
import CGNS
import numarray
from datetime import *
from time import *

# to see how to use sets

set0 = None
if (sys.version_info < (2, 4)):
    import sets
    set0 = sets.Set([1])
else:
    set0 = set([1])
    
# some string constants

thefmt = "%m-%d-%Y %H:%M:%S"

# -------------------------------------------------------------
# Block
# -------------------------------------------------------------
class Block(object):

    # -------------------------------------------------------------
    # Block.__init__()
    #
    # The cgns file here needs to be the MASS2 "grid.cgns" file, which
    # is expected to have a FlowSolution node named "GridMetrics"
    # -------------------------------------------------------------
    def __init__(self, cgns, b, z, ir, jr):
        zinfo = cgns.zoneread(b, z)
        self.imax = zinfo[3][2]
        self.jmax = zinfo[3][3]
        self.iriver = ir
        self.jriver = jr

        self.cellarea = None

        nsols = cgns.nsols(b, z)
        for s in range(1, nsols+1):
            (sloc, sname) = cgns.solinfo(b, z, s)
            if (sname == "GridMetrics"):
                self.cellarea = self.readfld(cgns, b, z, s, "hp1")
                tmp = self.readfld(cgns, b, z, s, "hp2")
                self.cellarea *= tmp
                
        self.segment = numarray.array(numarray.ones((self.jmax, self.imax), numarray.Int32))
        self.segmentlist = set0
        self.lastriver = None
        self.river = None

    # -------------------------------------------------------------
    # Block.readseg()
    #
    # Reads segment number from a CSV file 
    # -------------------------------------------------------------
    def readseg(self, fname):
        f = open(fname, mode='r')
        self.segmentlist.clear()
        lnum = 0;
        for l in f:
            lnum += 1
            if lnum == 1:
                continue
            l.rstrip();
            fld = l.split(',')
            blk = int(fld[0])
            i = int(fld[1])
            j = int(fld[2])
            seg = 0
            quad = 0
            try:
                seg = int(fld[3])
            except:
                sys.stderr.write("%s: warning: segment for (%d,%d,%d) set to 0\n" %
                                 (program, blk, i, j))
            try:
                quad = int(fld[4])
            except:
                sys.stderr.write("%s: warning: quadrant for (%d,%d,%d) set to 0\n" %
                                 (program, blk, i, j))

            # use quadrant instead of segment
            self.segmentlist.add(quad)
            self.segment[j,i] = quad
            #self.segmentlist.add(seg)
            #self.segment[j,i] = seg
        return

    # -------------------------------------------------------------
    # Block.readfld()
    #
    # Arrays read from the CGNS file need to be reshaped.  Use this
    # method to be consistent.
    # -------------------------------------------------------------
    def readfld(self, cgns, b, z, s, fname):
        tmp = cgns.fieldread(b, z, s, fname,
                               CGNS.RealDouble,
                               (1,1,0),
                               (self.imax, self.jmax, 0))
        result = numarray.reshape(tmp, (self.jmax, self.imax) )
        return result

    # -------------------------------------------------------------
    # Block.writefld()
    #
    # Arrays written to the CGNS file need to be reshaped.  Use this
    # method to be consistent.
    # -------------------------------------------------------------
    def writefld(self, cgns, b, z, s, fname, fld):
        tmp = numarray.reshape(fld, (self.imax, self.jmax))
        cgns.fieldwrite(b, z, s, CGNS.RealDouble, fname, tmp)
        return

    # -------------------------------------------------------------
    # Block.flood()
    # -------------------------------------------------------------
    def flood(self, i, j, isdry):

        if (i < 0 or i > self.imax - 1 or j < 0 or j > self.jmax - 1):
            return

        if (isdry[j, i] < 0.5) and (self.river[j, i] < 0.5):
            self.river[j, i] = 1
            self.flood(i + 1, j    , isdry)
            self.flood(i - 1, j    , isdry)
            self.flood(i    , j + 1, isdry)
            self.flood(i    , j - 1, isdry)
        return

    # -------------------------------------------------------------
    # Block.in_river()
    # -------------------------------------------------------------
    def in_river(self, cgns, b, z, s, modify):

        isdry = self.readfld(cgns, b, z, s, "isdry")

        if self.river != None:
            self.lastriver = self.river

        self.river = numarray.array(numarray.zeros(isdry.shape, numarray.Float))

        self.flood(self.iriver, self.jriver, isdry)

        if (modify):
            self.writefld(cgns, b, z, s, "InRiver", self.river)
        
        if self.lastriver == None:
            self.lastriver = self.river

        outriver = numarray.logical_not(self.river)
        outriver = numarray.logical_and(outriver, self.lastriver)
        strand = numarray.logical_and(outriver, isdry)
        iswet = numarray.logical_not(isdry)
        entrap = numarray.logical_and(outriver, iswet)

        if (modify):
            self.writefld(cgns, b, z, s, "StrandingArea", strand)
            self.writefld(cgns, b, z, s, "EntrappedArea", entrap)

        allstats = []

        for seg in self.segmentlist:
            cellarea = numarray.where(self.segment == seg, 1.0, 0.0)
            cellarea *= self.cellarea
            
            stats = {}
            stats['segment'] = seg

            # areas are reported in ha = 9.290304e-06*ft^2
            area = self.river*cellarea
            areasum = numarray.sum(numarray.sum(area))*9.290304e-06
            # stats['riverchange'] = areasum - stats['river']
            stats['river'] = areasum

            area = strand*cellarea
            areasum = numarray.sum(numarray.sum(area))*9.290304e-06
            stats['strand'] = areasum

            area = entrap*cellarea
            areasum = numarray.sum(numarray.sum(area))*9.290304e-06
            stats['entrap'] = areasum

            allstats.append(stats)

        return allstats

# -------------------------------------------------------------
# scanlist
# -------------------------------------------------------------
def scanlist(file):
    files = []
    start = []
    end = []
    for l in file:
        l.rstrip()
        l.lstrip()
        idx = l.find("#")
        if (idx >= 0):
            l = l[0:idx-1]
        if (len(l) == 0): continue

        fld = l.split()
        name = fld[1]
        sidx = int(fld[2])
        if (len(files) == 0):
            files.append(name)
            start.append(sidx)
            end.append(sidx)
        elif (name != files[-1]):
            files.append(name)
            start.append(sidx)
            end.append(sidx)
        elif (name == files[-1]):
            end[-1] = sidx
    return (files, start, end)

# -------------------------------------------------------------
# datetime_from_string
# -------------------------------------------------------------
def datetime_from_string(s):
    lt = strptime(s, thefmt)
    now = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday, lt.tm_hour, lt.tm_min, 0, 0)
    return now
    
    

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])
usage = "usage: " + program

startdate = datetime(1900, 1, 1)
enddate = datetime(3000, 1, 1)

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
usage = "Usage: %prog [options] grid.cgns plot0.cgns [plot1.cgns ...]"
parser = OptionParser(usage=usage)

parser.add_option("-v", "--verbose",
                  dest="verbose", action="store_true", default=False,
                  help="show what's going on")

parser.add_option("-I", "--iriver", type="int",
                  dest="iriver", action="store",
                  metavar="i",
                  help="The i index of the cell always in the river")

parser.add_option("-J", "--jriver", type="int",
                  dest="jriver", action="store",
                  metavar="j",
                  help="The j index of the cell always in the river")

parser.add_option("-S", "--segments", type="string",
                  dest="segments", action="store",
                  help="name of file containing i,j to segment mapping")

parser.add_option("-M", "--modify",
                  dest="modify", action="store_true", default=False,
                  help="write area fields to input CGNS files")

parser.add_option("-L", "--list", type="string",
                  dest="sollist", action="store",
                  help="take solutions from a file instead of the command line")

parser.add_option("-b", "--begin", type="string",
                  dest="dbegin", action="store",
                  help="begin processing with specified date time (mm-dd-YYYY HH:MM:SS)")

parser.add_option("-e", "--end", type="string",
                  dest="dend", action="store",
                  help="end processing with specified date time (mm-dd-YYYY HH:MM:SS)")

parser.add_option("-o", "--output", type="string",
                  dest="output", action="store")

(options, args) = parser.parse_args()

doverbose = options.verbose

if (len(args) < 1):
    parser.print_help()
    sys.exit(3)
    
gfile = args[0]


if options.sollist:
    slist = open(options.sollist, mode="r")
    (files, startidx, endidx) = scanlist(slist)
    print (files, startidx, endidx)
else:
    files = args[1:]
    startidx = None
    endidx = None

if (len(files) == 0):
    sys.stderr.write("%s: error: no solution files specified\n" % (program))
    parser.print_help()
    sys.exit(3)

iriver = int(options.iriver)
jriver = int(options.jriver)

if options.segments:
    segfile = options.segments
else:
    segfile = None

if options.output:
    output = open(options.output, mode='w')
else:
    output = sys.stdout

domodify = options.modify

if options.dbegin:
    try:
        startdate = datetime_from_string(options.dbegin)
        startdate -= timedelta(0, 3600)
    except:
        sys.stderr.write("Error parsing --begin argument (%s)\n" % (options.dbegin))
        sys.exit(3)
    
if options.dend:
    try:
        enddate = datetime_from_string(options.dend)
    except:
        sys.stderr.write("Error parsing --end argument (%s)\n" % (options.dend))
        sys.exit(3)

base = 1
zone = 1

if doverbose:
    sys.stderr.write("%s: info: in river cell index = (%d, %d)\n" %
                     (program, iriver, jriver))

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

sys.setrecursionlimit(1000000)


if doverbose:
    sys.stderr.write("%s: info: extracting geometry information from %s\n" %
                         (program, gfile));
cgns = CGNS.pyCGNS(gfile, CGNS.MODE_READ)
blk = Block(cgns, base, zone, iriver, jriver)
cgns.close()

if segfile:
    blk.readseg(segfile)

# print files

done = None
first = 1
for f in files:
    if doverbose:
        sys.stderr.write("%s: info: processing file %s\n" %
                         (program, f));

    if domodify:
        cgns = CGNS.pyCGNS(f, CGNS.MODE_MODIFY)
    else:
        cgns = CGNS.pyCGNS(f, CGNS.MODE_READ)
        
    if doverbose:
        sys.stderr.write("%s: info: zone has %d x %d cells\n" %
                         (program, blk.imax, blk.jmax))

    idx0 = 1
    idx1 = cgns.nsols(base, zone)
    if (startidx):
        idx0 = startidx.pop(0)
        idx1 = endidx.pop(0)

    if doverbose:
        sys.stderr.write("%s: info: %s: extracting solutions %d to %d\n" %
                         (program, f, idx0, idx1))

    for sidx in range(idx0, idx1+1):
        (sloc, sname) = cgns.solinfo(base, zone, sidx)
        thedate = datetime_from_string(sname)

        sstat = "skipped"
        if (startdate <= thedate and thedate <= enddate):
            sstat = "processed"
            allstats = blk.in_river(cgns, 1, 1, sidx, domodify)
            # print allstats
            if (not first):
                for s in allstats:
                    output.write("%s %3d %10.6e %10.6e %10.6e\n" %
                                 (sname, s['segment'], s['river'],
                                  s['strand'], s['entrap']))
            first = None
        elif (thedate > enddate):
            done = 1
            break
        if doverbose:
            sys.stderr.write("%s: info: %s solution %d \"%s\"\n" %
                         (program, sstat, sidx, sname))
    cgns.close()
    if (done):
        break
output.close()
    
