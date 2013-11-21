set term post enh eps color dashed "Helvetica" 18

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"
set key left

set format y "%.2f"
set ytics nomirror
set ylabel "Hourly Precipitation, in"
set format y2 "%.2f"
set y2tics nomirror
set y2label "Cumulative Precipitation, ft"

set format x "%d%b\n%Y"
set xrange ["01-01-1992 00:00:00":"01-01-1993 00:00:00"]

plot 'precip.dat' using 1:3 axes x1y1 title "Hourly" with impulses ls 1, \
     'cumulative_precip.dat' using 1:3 axes x1y2 title "Cumulative" with lines ls 3