# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: Thu Jul  6 21:51:04 2000 by William A. Perkins <perk@localhost>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

u = 2.0
Co = 10.0
Tp = 12 * 60
C(t,Tp) = (t < 0) ? 0 : ((t < Tp) ? (1 - (Tp-t)/Tp)*Co : ((t < 2*Tp) ? ((2*Tp - t)/Tp)*Co : 0))

set samples 2000
set format x "%.1f"
set xrange [0:10000]
set xlabel 'Longitudinal Distance, ft'
set format y "%.1f"
set ylabel 'Concentration'
set pointsize 0.5
# set timestamp
set key below

plot C(24*60 - x/u,Tp) title 'Translated BC @ t = 24 min' with lines 1, \
     '< perl ../../../scripts/mass2slice.pl -i -t 8 plot.nc temperature 1 6' using 3:4 title 'Simulated @ t = 24 min' with points 1, \
     C(48*60 - x/u,Tp) title 'Translated BC @ t = 48 min' with lines 3, \
     '< perl ../../../scripts/mass2slice.pl -i -t 10 plot.nc temperature 1 6' using 3:4 title 'Simulated @ t = 48 min' with points 3, \
     C(72*60 - x/u,Tp) title 'Translated BC @ t = 72 min' with lines 4, \
     '< perl ../../../scripts/mass2slice.pl -i -t 12 plot.nc temperature 1 6' using 3:4 title 'Simulated @ t = 72 min' with points 4


