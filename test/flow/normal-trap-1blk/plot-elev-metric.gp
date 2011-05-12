# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Wed May 11 07:29:41 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps mono dashed "Helvetica" 24

set samples 2000
set ylabel 'Water Surface Elevation, m'
set auto y
set format y '%0.1f'

set xlabel 'Centerline Distance, m'
set pointsize 0.5
set xrange [0:310]

set key at 250,2.5

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc wsel 1 12' using ($3*0.3048):($4*0.3048) title 'Initial Conditions' with lines ls 3, \
     '<perl ../../../scripts/mass2slice.pl -t 2 -i plot.nc wsel 1 12' using ($3*0.3048):($4*0.3048) title 'Steady State' with linespoints ls 1, \
     '<perl ../../../scripts/mass2slice.pl -i plot.nc zbot 1 12' using ($3*0.3048):(($4+4)*0.3048) title 'Analytic' with lines ls 6, \
     '<perl ../../../scripts/mass2slice.pl -i plot.nc zbot 1 12' using ($3*0.3048):($4*0.3048) title 'Bottom' with lines ls 7
