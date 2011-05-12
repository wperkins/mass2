# -------------------------------------------------------------
# file: bcplot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 24, 2004 by William A. Perkins
# Last Change: Wed Mar 24 12:14:20 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
#$Id$

set term post eps mono dashed "Helvetica" 24

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set format x '%H:%M'
set xrange ['01-01-1997 05:30:00':'01-01-1997 07:00:00']
set xlabel 'Time'
set xtics ('01-01-1997 05:30:00', \
           '01-01-1997 05:45:00', \
           '01-01-1997 06:00:00', \
           '01-01-1997 06:15:00', \
           '01-01-1997 06:30:00', \
           '01-01-1997 06:45:00', \
           '01-01-1997 07:00:00')

set format y '%.1f'
set ylabel 'Concentration'

set nokey
plot 'conc-bc.dat' using 1:3 with lines ls 1
