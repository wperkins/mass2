# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: Thu Feb  1 18:57:22 2007 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

set term postscript eps mono dashed "Helvetica" 24

u = 2.0
Co = 10.0
Tp = 12 * 60
C(t,Tp) = (t < 0) ? 0 : ((t < Tp) ? (1 - (Tp-t)/Tp)*Co : ((t < 2*Tp) ? ((2*Tp - t)/Tp)*Co : 0))

set samples 2000
set format x "%.0f"
set xrange [0:3100]
set xlabel 'Longitudinal Distance, m'
set format y "%.1f"
set ylabel 'Concentration'
set pointsize 0.5
# set timestamp
set nokey

plot C(24*60 - x/0.3048/u,Tp) title 'Advected BC' with lines 1, \
     '< perl ../../../scripts/mass2slice.pl -i -t 8 plot.nc stuff 1 6' using ($3*0.3048):4 title 'Simulated' with points 7, \
     C(48*60 - x/0.3048/u,Tp) notitle with lines 1, \
     '< perl ../../../scripts/mass2slice.pl -i -t 10 plot.nc stuff 1 6' using ($3*0.3048):4 notitle with points 7, \
     C(72*60 - x/0.3048/u,Tp) notitle with lines 1, \
     '< perl ../../../scripts/mass2slice.pl -i -t 12 plot.nc stuff 1 6' using ($3*0.3048):4 notitle with points 7


