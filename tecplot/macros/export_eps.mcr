#!MC 700
# -------------------------------------------------------------
# file: export_eps.mcr
# This script is for exporting a ready-made layout to EPS while in
# batch mode.  To run do the following:
#
#   tecplot -b -p export_eps.mcr -y export-name.eps layout-name.lay
#
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created September 13, 2001 by William A. Perkins
# Last Change: Thu Sep 13 12:20:24 2001 by William A. Perkins <perk@gehenna.pnl.gov>
# -------------------------------------------------------------

$!EXPORTSETUP 
  EXPORTFORMAT = EPS
  PRECISION = 3
  PALETTE = COLOR
  EPSPREVIEWIMAGE { IMAGETYPE = NONE }
$!EXPORT  
  APPEND = FALSE
