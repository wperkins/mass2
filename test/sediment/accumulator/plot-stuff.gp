# -------------------------------------------------------------
# file: plot-particulate.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 14, 2000 by William A. Perkins
# Last Change: Wed Nov 15 11:46:54 2000 by William A. Perkins <perk@gehenna.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set ylabel 'Mass per ft^2 bed area'
set ytics nomirror
set y2label 'Concentration, mass per ft^3'
set y2tics nomirror

set title 'Gage @NUM@'

set pointsize 1.0
set key below

plot '<perl ../../../scripts/mass2gage.pl -g @NUM@ -v stuff-silt-bed gage.avg.nc' using 1:3 title 'Particulate (Gage)' with lines 1, \
     '<perl ../../../scripts/mass2gage.pl -g @NUM@ -v stuff-bed gage.avg.nc' using 1:3 title 'Dissolved (Gage)' with lines 3, \
     '<perl ../../../scripts/mass2gage.pl -g @NUM@ -v stuff-pore gage.avg.nc' using 1:3 axes x1y2 title 'Pore Conc. (Gage)' with lines 4, \
     '<perl ../../../scripts/mass2slice.pl -p plot.inst.nc stuff-silt-bed @BLK@ @X@ @Y@' using 1:5 title 'Particulate (Inst. Plot)' with points 1, \
     '<perl ../../../scripts/mass2slice.pl -p plot.inst.nc stuff-bed @BLK@ @X@ @Y@' using 1:5 title 'Dissolved (Inst. Plot)' with points 3, \
     '<perl ../../../scripts/mass2slice.pl -p plot.inst.nc stuff-pore @BLK@ @X@ @Y@' using 1:5 axes x1y2 title 'Pore Conc. (Inst. Plot)' with points 4, \
     '<perl ../../../scripts/mass2slice.pl -p plot.avg.nc stuff-silt-bed @BLK@ @X@ @Y@' using 1:5 title 'Particulate (Avg. Plot)' with points 10, \
     '<perl ../../../scripts/mass2slice.pl -p plot.avg.nc stuff-bed @BLK@ @X@ @Y@' using 1:5 title 'Dissolved (Avg. Plot)' with points 12, \
     '<perl ../../../scripts/mass2slice.pl -p plot.avg.nc stuff-pore @BLK@ @X@ @Y@' using 1:5 axes x1y2 title 'Pore Conc. (Avg. Plot)' with points 13
