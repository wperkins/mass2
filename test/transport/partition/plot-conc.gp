# -------------------------------------------------------------
# file: plot-conc.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created August 31, 2000 by William A. Perkins
# Last Change: 2014-06-24 09:09:35 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps enhanced mono dashed "Helvetica" 24

set samples 2000
set ylabel 'Concentration, kg/m^{3}'
set ytics nomirror
set format y '%.1e'
set auto y
set xlabel 'Longitudinal Distance, m'
set xrange [0:3100]
set nogrid
set mytics
set pointsize 0.5
set key

plot "<awk 'NR > 20 {print $1, $21}' @probe@" using ($1*0.3048):2 title 'Dissolved' with linespoints ls 1, \
     "<awk 'NR > 20 {print $1, $23}' @probe@" using ($1*0.3048):2 title 'Particulate' with linespoints ls 3
