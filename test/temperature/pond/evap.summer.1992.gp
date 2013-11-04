#!/usr/bin/gnuplot -persist
#
#    
#    	G N U P L O T
#    	Version 4.0 patchlevel 0
#    	last modified Thu Apr 15 14:44:22 CEST 2004
#    	System: Linux 2.6.9-89.0.7.ELsmp
#    
#    	Copyright (C) 1986 - 1993, 1998, 2004
#    	Thomas Williams, Colin Kelley and many others
#    
#    	This is gnuplot version 4.0.  Please refer to the documentation
#    	for command syntax changes.  The old syntax will be accepted
#    	throughout the 4.0 series, but all save files use the new syntax.
#    
#    	Type `help` to access the on-line reference manual.
#    	The gnuplot FAQ is available from
#    		http://www.gnuplot.info/faq/
#    
#    	Send comments and requests for help to
#    		<gnuplot-info@lists.sourceforge.net>
#    	Send bugs, suggestions and mods to
#    		<gnuplot-bugs@lists.sourceforge.net>
#    
# set terminal x11 0
# set output
set terminal postscript eps enhanced color dashed "Helvetica" 14 
unset clip points
set clip one
unset clip two
set bar 1.000000
set border 31 lt -1 lw 1.000
set xdata time
set ydata
set zdata
set x2data
set y2data
set timefmt x "%m-%d-%Y %H:%M:%S"
set timefmt y "%m-%d-%Y %H:%M:%S"
set timefmt z "%m-%d-%Y %H:%M:%S"
set timefmt x2 "%m-%d-%Y %H:%M:%S"
set timefmt y2 "%m-%d-%Y %H:%M:%S"
set timefmt cb "%m-%d-%Y %H:%M:%S"
set boxwidth
set style fill empty border
set dummy x,y
set format x "%b%d\n%Y"
set format y "% g"
set format x2 "% g"
set format y2 "% g"
set format z "% g"
set format cb "% g"
set angles radians
unset grid
set key title ""
unset key
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
set xzeroaxis lt 0 lw 1.000
set yzeroaxis lt -2 lw 1.000
set x2zeroaxis lt -2 lw 1.000
set y2zeroaxis lt -2 lw 1.000
set tics in
set ticslevel 0.5
set ticscale 1 0.5
set mxtics default
set mytics default
set mztics default
set mx2tics default
set my2tics default
set mcbtics default
set xtics border mirror norotate autofreq 
set ytics border mirror norotate autofreq 
set ztics border nomirror norotate autofreq 
set nox2tics
set noy2tics
set cbtics border mirror norotate autofreq 
set title "" 
set timestamp "%a %b %d %H:%M:%S %Y" bottom norotate 
set rrange [ * : * ] noreverse nowriteback  # (currently [0.00000:10.0000] )
set trange [ * : * ] noreverse nowriteback  # (currently ["31/12/99,23:59":"01/01/00,00:00"] )
set urange [ * : * ] noreverse nowriteback  # (currently ["31/12/99,23:59":"01/01/00,00:00"] )
set vrange [ * : * ] noreverse nowriteback  # (currently ["31/12/99,23:59":"01/01/00,00:00"] )
set xlabel "" 
set x2label "" 
set xrange [ "04-01-1992 00:00:00" : "10-01-1992 00:00:00" ] noreverse nowriteback
set x2range [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set ylabel "Evaporation Rate, mm/day" 
set y2label "" 
set yrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set y2range [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set zlabel "" 
set zrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set cblabel "" 
set cbrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set zero 1e-08
set lmargin -1
set bmargin -1
set rmargin -1
set tmargin -1
set locale "C"
set pm3d at s
set pm3d scansautomatic flush begin noftriangles nohidden3d explicit corners2color mean
set palette positive nops_allcF maxcolors 0 gamma 1.5 color model RGB 
set palette rgbformulae 7, 5, 15
set colorbox default
set colorbox vertical origin 0.9,0.2 size 0.1,0.63 bdefault
set loadpath 
set fontpath 
set fit noerrorvariables
plot "gage_1.out" using 1:($11*25.4) with lines ls 1
#    EOF
