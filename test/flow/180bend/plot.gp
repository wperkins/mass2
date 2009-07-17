#!/sw/bin/gnuplot -persist
#
#    
#    	G N U P L O T
#    	Version 4.2 patchlevel 4 
#    	last modified Sep 2008
#    	System: Darwin 8.11.0
#    
#    	Copyright (C) 1986 - 1993, 1998, 2004, 2007, 2008
#    	Thomas Williams, Colin Kelley and many others
#    
#    	Type `help` to access the on-line reference manual.
#    	The gnuplot FAQ is available from http://www.gnuplot.info/faq/
#    
#    	Send bug reports and suggestions to <http://sourceforge.net/projects/gnuplot>
#    
set terminal post eps dashed "Helvetica" 18
# set output
unset clip points
set clip one
unset clip two
set bar 1.000000
set xdata
set ydata
set zdata
set x2data
set y2data
set timefmt x "%d/%m/%y,%H:%M"
set timefmt y "%d/%m/%y,%H:%M"
set timefmt z "%d/%m/%y,%H:%M"
set timefmt x2 "%d/%m/%y,%H:%M"
set timefmt y2 "%d/%m/%y,%H:%M"
set timefmt cb "%d/%m/%y,%H:%M"
set boxwidth
set style fill  empty border
set dummy x,y
set format x "%.1f"
set format y "%.1f"
set format x2 "% g"
set format y2 "% g"
set format z "% g"
set format cb "% g"
set angles radians
unset grid
set key title ""
unset label
unset arrow
unset style line
unset style arrow
unset logscale
set offsets 0, 0, 0, 0
set pointsize 1
set encoding default
unset polar
unset parametric
unset decimalsign
set view 60, 30, 1, 1
set samples 100, 100
set isosamples 10, 10
set surface
unset contour
set clabel '%8.3g'
set mapping cartesian
set datafile separator whitespace
unset hidden3d
set cntrparam order 4
set cntrparam linear
set cntrparam levels auto 5
set cntrparam points 5
set size ratio 0 1,1
set origin 0,0
set style data points
set style function lines
set xzeroaxis linetype -2 linewidth 1.000
set yzeroaxis linetype -2 linewidth 1.000
set x2zeroaxis linetype -2 linewidth 1.000
set y2zeroaxis linetype -2 linewidth 1.000
set ticslevel 0.5
set mxtics default
set mytics default
set mztics default
set mx2tics default
set my2tics default
set mcbtics default
set xtics autofreq 
set ytics autofreq 
set ztics autofreq 
set nox2tics
set noy2tics
set cbtics autofreq 
set title "" 
set timestamp bottom 
set timestamp "" 
set rrange [ * : * ] noreverse nowriteback  # (currently [0.00000:10.0000] )
set trange [ * : * ] noreverse nowriteback  # (currently [-5.00000:5.00000] )
set urange [ * : * ] noreverse nowriteback  # (currently [-5.00000:5.00000] )
set vrange [ * : * ] noreverse nowriteback  # (currently [-5.00000:5.00000] )
set xlabel "Channel center-line distance, m" 
set x2label "" 
set xrange [ 5.50000 : 9.50000 ] noreverse nowriteback
set x2range [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set ylabel "Depth, cm" 
set y2label "" 
set yrange [ 4.80000 : 6.80000 ] noreverse nowriteback
set y2range [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set zlabel "" 
set zrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set cblabel "" 
set cbrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set zero 1e-08
set lmargin  -1
set bmargin  -1
set rmargin  -1
set tmargin  -1
set locale "C"
set pm3d explicit at s
set pm3d scansautomatic
set palette positive nops_allcF maxcolors 0 gamma 1.5 color model RGB 
set palette rgbformulae 7, 5, 15
set colorbox default
set loadpath 
set fontpath 
set fit noerrorvariables
set arrow 1 from 6,6.4 to 6,6.1 lt 1
set label 1 "Bend Exit" at 6,6.45 center font "Helvectica,12" nopoint
set arrow 2 from 8.513,6.4 to 8.513,6.1 lt 1
set label 2 "Bend Entrance" at 8.513,6.45 center font "Helvectica,12" nopoint
plot 'depth.txt' using ($3*0.3048):($8*30.48) title 'Simulated MASS2' w lines 1, \
     'depth.txt' using ($3*0.3048):($12*30.48) notitle w lines 1, \
     'Observed/molls_stage_sim_inner.dat' using ($1*0.8+5.5):($2) title "Simulated, Molls and Chauhdry (1995)" w lines 2, \
     'Observed/molls_stage_sim_outer.dat' using ($1*0.8+5.5):($2) notitle w lines 2, \
     'Observed/molls_stage_obs_outer.dat' using 1:2 title "Observed" w points 7, \
     'Observed/molls_stage_obs_inner.dat' using 1:2 notitle w points 7
#    EOF
