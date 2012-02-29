#!/usr/bin/env python
# -*- mode: python;-*-
# -------------------------------------------------------------
# file: mass2_setup.py
#
# A script to set up a reasonable plot in VisIt
#
# run like this: visit -s mass2_setup.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 17, 2012 by William A. Perkins
# Last Change: Tue Jan 17 14:50:15 2012 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

import getopt, sys, os

# -------------------------------------------------------------
# 2D Attributes
# -------------------------------------------------------------

# fill entire window always

att = View2DAttributes()
att.viewportCoords = (0.0, 1.0, 0.0, 1.0)
att.fullFrameActivationMode = att.On
fullFrameAutoThreshold = 100
SetView2D(att)

AnnotationAtts = AnnotationAttributes()
AnnotationAtts.axes2D.visible = 0
AnnotationAtts.userInfoFlag = 0
AnnotationAtts.databaseInfoFlag = 0
AnnotationAtts.legendInfoFlag = 1
SetAnnotationAttributes(AnnotationAtts)

# -------------------------------------------------------------
# Open database
# -------------------------------------------------------------

OpenDatabase("plot000.cgns")

# -------------------------------------------------------------
# Add a plot for a start
# -------------------------------------------------------------

# depth
AddPlot("Pseudocolor", "depth", 1, 1)
SetActivePlots(0)

# blank out dry cells
AddOperator("Threshold", 1)
ThresholdAtts = ThresholdAttributes()
ThresholdAtts.listedVarNames = ("isdry")
ThresholdAtts.lowerBounds = (-1e+37)
ThresholdAtts.upperBounds = (0.5)
SetOperatorOptions(ThresholdAtts, 1)

DrawPlots()
RecenterView()

