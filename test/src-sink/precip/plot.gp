set term post enh eps color dashed "Helvetica" 18

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"
set key left

set format y "%.2f"
set ytics nomirror
set ylabel "Water Surface Elevation, ft"
set format y2 "%.2f"
set y2tics nomirror
set y2label "Cumulative Precipitation, ft"

set format x "%d%b\n%Y"
set xrange ["01-01-1992 00:00:00":"01-01-1993 00:00:00"]
set yrange [5.0:5.5]

plot 'gage_A.out' using 1:5 axes x1y1 title "Stage @ Location A"  with lines ls 1, \
     'gage_C.out' using 1:5 axes x1y1 title "Stage @ Location C"  with lines ls 3, \
     'cumulative_precip.dat' using 1:3 axes x1y2 title "Precipitation" with lines ls 7