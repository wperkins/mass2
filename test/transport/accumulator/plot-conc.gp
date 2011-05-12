# -------------------------------------------------------------
# file: plot-conc.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November  7, 2000 by William A. Perkins
# Last Change: Tue Nov  7 10:02:53 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

set terminal postscript eps color dashed "Helvetica" 14                         
set ylabel 'Concentration'                                                      
set format y '%.1f'                                                             
set auto y                                                                      
set xlabel 'Longitudinal Distance, feet'                                        
set grid y                                                                      
set grid x                                                                      
set pointsize 0.5                                                               
set key below                                                                   
                                                                                
plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.inst.nc temperature 1 5' using 3:4 title 'Instaneous - Initial' with points ls 1, \
     '<perl ../../../scripts/mass2slice.pl -t 2 -i plot.inst.nc temperature 1 5' using 3:4 title 'Instaneous - Final' with points ls 3, \
     '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.avg.nc temperature 1 5' using 3:4 title 'Averaged - Final' with lines ls 4, \
     '<perl ../../../scripts/mass2slice.pl -t 2 -i plot.avg.nc temperature 1 5' using 3:4 title 'Averaged - Final' with lines ls 5
