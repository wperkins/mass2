# -------------------------------------------------------------
# file: long-plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 24, 2003 by William A. Perkins
# Last Change: Fri Oct 31 12:51:06 2008 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------

set term post eps enhanced mono dashed "Helvetica" 24

u = 0.83
b = 0.492
xb = 5.91

yob(y) = y/b
xob(x) = (x-xb)/b

set xlabel 'x/b'
set format x "%0.1f"
set ylabel 'U/U_{o}'
set format y "%0.1f"
set yrange [0.7:1.8]

set xrange [-6:1]

set title "y/b = 1.0"
set output 'long-1.0-@CASE@.eps'
plot '<perl ../../../scripts/mass2slice.pl -i -l plot-@CASE@.nc vmag 1 8' using (xob($3)):($4/u) title 'Simulated' with lines 1, \
     'observed/spur-all-a-molls.dat' using (($1-1.8)/0.152):2 title 'Molls and Chaudhry (1995)' with lines 2, \
     'observed/tings-fig7-a-sim.dat' title 'Tingsanchali and Maheswaran (1990)' with lines 7, \
     'observed/tings-fig7-a-exp.dat' title 'Experimental' with points 7

set nokey

set title "y/b = 1.5"
set output 'long-1.5-@CASE@.eps'
plot '<perl ../../../scripts/mass2slice.pl -i -l plot-@CASE@.nc vmag 1 12' using (xob($3)):($4/u) title 'Simulated' with lines 1, \
     'observed/spur-all-b-molls.dat' using (($1-1.8)/0.152):2 title 'Molls and Chaudhry (1995)' with lines 2, \
     'observed/tings-fig7-b-sim.dat' title 'Simulated by ...' with lines 7, \
     'observed/tings-fig7-b-exp.dat' title 'Experimental' with points 7

set xrange [-6:10]

set output 'long-2.0-@CASE@.eps'
set title "y/b = 2.0"
plot '<perl ../../../scripts/mass2slice.pl -i -l plot-@CASE@.nc vmag 1 15' using (xob($3)):($4/u) title 'Simulated' with lines 1, \
     'observed/spur-all-c-molls.dat' using (($1-1.8)/0.152):2 title 'Molls and Chaudhry (1995)' with lines 2, \
     'observed/tings-fig7-c-sim.dat' title 'Simulated by ...' with lines 7, \
     'observed/tings-fig7-c-exp.dat' title 'Experimental' with points 7

set output 'long-3.0-@CASE@.eps'
set title "y/b = 3.0"
plot '<perl ../../../scripts/mass2slice.pl -i -l plot-@CASE@.nc vmag 1 23' using (xob($3)):($4/u) title 'Simulated' with lines 1, \
     'observed/spur-all-d-molls.dat' using (($1-1.8)/0.152):2 title 'Molls and Chaudhry (1995)' with lines 2, \
     'observed/tings-fig7-d-sim.dat' title 'Simulated by ...' with lines 7, \
     'observed/tings-fig7-d-exp.dat' title 'Experimental' with points 7

set output 'long-4.0-@CASE@.eps'
set title "y/b = 4.0"
plot '<perl ../../../scripts/mass2slice.pl -i -l plot-@CASE@.nc vmag 1 30' using (xob($3)):($4/u) title 'Simulated' with lines 1, \
     'observed/spur-all-e-molls.dat' using (($1-1.8)/0.152):2 title 'Molls and Chaudhry (1995)' with lines 2, \
     'observed/tings-fig7-e-sim.dat' title 'Simulated by ...' with lines 7, \
     'observed/tings-fig7-e-exp.dat' title 'Experimental' with points 7

set output
