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
# Last Change: 2018-06-12 07:52:41 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

import getopt, sys, os

# -------------------------------------------------------------
# 2D Attributes
# -------------------------------------------------------------

# fill entire window always

att = View2DAttributes()
att.viewportCoords = (0.0, 1.0, 0.0, 1.0)
att.fullFrameActivationMode = att.Auto
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
pc = PseudocolorAttributes()
pc.scaling = pc.Log
SetActivePlots(0)
SetPlotOptions(pc)

# velocity vectors
AddPlot("Vector", "Velocity", 1, 1)
vp = VectorAttributes()
vp.colorByMag = 0
vp.scale = 0.025
vp.useLegend = 0
vp.useStride = 1
SetActivePlots(1)
SetPlotOptions(vp)

SetActivePlots((0,1))

# blank out dry cells
AddOperator("Threshold", 1)
ThresholdAtts = ThresholdAttributes()
ThresholdAtts.listedVarNames = ("isdry")
ThresholdAtts.lowerBounds = (-1e+37)
ThresholdAtts.upperBounds = (0.5)
SetOperatorOptions(ThresholdAtts, 1)

DrawPlots()
RecenterView()

