# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: Thu Feb  1 19:00:05 2007 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$


set term postscript eps mono dashed "Helvetica" 24

# set title "TheTitle"

u = 2.0
D = 30.0
Co = 10.0
C(x,t) = (Co/2)*(erfc((x-u*t)/sqrt(4*D*t))+erfc((x+u*t)/sqrt(4*D*t))*exp(u*x/D))

set format x "%.0f"
set xlabel 'Longitudinal Distance, m'
set format y "%.0f"
set ylabel 'Concentration'
set xrange [0:3100]
set pointsize 0.5
#set timestamp
set nokey

set arrow from first 4000*0.3048, graph 0.0 to first 4000*0.3048, graph 1.0 nohead lt 7
set arrow from first 6000*0.3048, graph 0.0 to first 6000*0.3048, graph 1.0 nohead lt 7

                                # If delta t is 18.0s (0.005 hr), t =
                                # 360.0 is 20 time steps, t = 1080.0
                                # is 60 time steps, and t = 1800.0 is
                                # 100 time steps

plot C(x/0.3048, 360.0) title "Analytic Solution" with lines 1, \
     C(x/0.3048, 1080.0) notitle with lines 1, \
     C(x/0.3048, 1800.0) notitle with lines 1, \
     C(x/0.3048, 2520.0) notitle with lines 1, \
     C(x/0.3048, 3240.0) notitle with lines 1, \
     '< perl ../../../scripts/mass2slice.pl -i -t 12 plot.nc stuff 1 6 2 6 3 6' using ($3*0.3048):4 title 'Simulated' with points 7, \
     '< perl ../../../scripts/mass2slice.pl -i -t 14 plot.nc stuff 1 6 2 6 3 6' using ($3*0.3048):4 notitle with points 7, \
     '< perl ../../../scripts/mass2slice.pl -i -t 16 plot.nc stuff 1 6 2 6 3 6' using ($3*0.3048):4 notitle with points 7, \
     '< perl ../../../scripts/mass2slice.pl -i -t 18 plot.nc stuff 1 6 2 6 3 6' using ($3*0.3048):4 notitle with points 7, \
     '< perl ../../../scripts/mass2slice.pl -i -t 20 plot.nc stuff 1 6 2 6 3 6' using ($3*0.3048):4 notitle with points 7


