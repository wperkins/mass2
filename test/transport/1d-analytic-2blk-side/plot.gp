# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: Thu Mar 25 15:06:20 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$


set term postscript eps color dashed "Helvetica" 14

set title "TheTitle"

u = 2.0
D = 30.0
Co = 10.0
C(x,t) = (Co/2)*(erfc((x-u*t)/sqrt(4*D*t))+erfc((x+u*t)/sqrt(4*D*t))*exp(u*x/D))

set format x "%.1f"
set xlabel 'Longitudinal Distance, ft'
set format y "%.1f"
set ylabel 'Concentration'
set xrange [0:10000]
set pointsize 0.5
#set timestamp
set key below

set arrow from 5000, 0.0 to 5000, 10.0 nohead lt 0 

                                # If delta t is 18.0s (0.005 hr), t =
                                # 360.0 is 20 time steps, t = 1080.0
                                # is 60 time steps, and t = 1800.0 is
                                # 100 time steps

plot C(x, 360.0) title "Analytic: t = 6.0 min", \
     C(x, 1080.0) title "Analytic: t = 18.0 min", \
     C(x, 1800.0) title "Analytic: t = 30.0 min", \
     C(x, 2520.0) title "Analytic: t = 42.0 min", \
     C(x, 3240.0) title "Analytic: t = 54.0 min", \
     '< perl ../../../scripts/mass2slice.pl -j -t 12 plot.nc stuff 1 6 2 6' using 3:4 title 'Simulated' with points ls 7, \
     '< perl ../../../scripts/mass2slice.pl -j -t 14 plot.nc stuff 1 6 2 6' using 3:4 notitle with points ls 7, \
     '< perl ../../../scripts/mass2slice.pl -j -t 16 plot.nc stuff 1 6 2 6' using 3:4 notitle with points ls 7, \
     '< perl ../../../scripts/mass2slice.pl -j -t 18 plot.nc stuff 1 6 2 6' using 3:4 notitle with points ls 7, \
     '< perl ../../../scripts/mass2slice.pl -j -t 20 plot.nc stuff 1 6 2 6' using 3:4 notitle with points ls 7


