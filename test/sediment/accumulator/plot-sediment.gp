# -*- gnuplot -*-----------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created September 12, 2000 by William A. Perkins
# Last Change: Wed Nov 15 11:51:38 2000 by William A. Perkins <perk@gehenna.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set ylabel 'Concentration'
set y2label 'Depth, feet'
set y2tics nomirror
set ytics nomirror

set title 'Gage @NUM@'

set pointsize 1.0
set key below

plot '<perl ../../../scripts/mass2gage.pl -g @NUM@ -v silt-bed gage.avg.nc' using 1:3 axes x1y1 title 'Sediment (Gage)' with lines 1, \
     '<perl ../../../scripts/mass2gage.pl -g @NUM@ -v beddepth gage.avg.nc' using 1:3 axes x1y2 title 'Bed Depth (Gage)' with lines 3, \
     '<perl ../../../scripts/mass2slice.pl -p plot.inst.nc silt-bed @BLK@ @X@ @Y@' using 1:5 axes x1y1 title 'Sediment (Inst. Plot)' with points 1, \
     '<perl ../../../scripts/mass2slice.pl -p plot.inst.nc beddepth @BLK@ @X@ @Y@' using 1:5 axes x1y2 title 'Bed Depth (Inst. Plot)' with points 3, \
     '<perl ../../../scripts/mass2slice.pl -p plot.avg.nc silt-bed @BLK@ @X@ @Y@' using 1:5 axes x1y1 title 'Sediment (Avg. Plot)' with points 10, \
     '<perl ../../../scripts/mass2slice.pl -p plot.avg.nc beddepth @BLK@ @X@ @Y@' using 1:5 axes x1y2 title 'Bed Depth (Avg. Plot)' with points 12
