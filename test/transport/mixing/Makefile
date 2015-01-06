# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 30, 2002 by William A. Perkins
# Last Change: 2014-08-14 07:27:32 d3g096
# -------------------------------------------------------------
# $Id$

MPIRUN = mpiexec
NPROC = 4
MODEL = ../../../mass2
TECPLOT = tecplot

EPSFILES = \
	mixing.eps \
	mixing-metric.eps \
	plot-lat-1.mix.eps \
	plot-lat-2.mix.eps \
	plot-long.mix.eps \
	# nomixing.eps \
	# plot-lat-1.nomix.eps \
	# plot-long.nomix.eps \
	# wall.eps \
	# plot-lat-1.wall.eps \
	# plot-long.wall.eps

MODELFILES = \
	bkgnd.dat \
	conc.dat \
	flow.dat \
	grid.dat \
	mass2_v027.base \
	stage.dat \
	scalar_bcspecs.dat \
	scalar_source.dat

all: $(EPSFILES)

# -------------------------------------------------------------
# the base case -- comparison to analytic solution
# -------------------------------------------------------------
plot.mix.cgns: $(MODELFILES) $(MODEL) bcspecs.base
	cp mass2_v027.base mass2.cfg
	cp bcspecs.base bcspecs.dat
	$(MPIRUN) -np $(NPROC) $(MODEL)
	mv plot000.cgns plot.mix.cgns

clean:: 
	rm -f plot.mix.cgns mass2_v027.cfg bcspecs.dat

mixing.lay: mixing.base.lay
	sed -e 's/plot000/plot.mix/g' mixing.base.lay > mixing.lay

mixing.eps: plot.mix.cgns mixing.lay
	$(TECPLOT) -b -y $@ -p ../../../tecplot/macros/export_eps.mcr mixing.lay 

mixing-metric.eps: plot.mix.cgns mixing.lay metric.mcr
	$(TECPLOT) -b -y $@ mixing.lay -p metric.mcr 

.PHONY : probe

probe1x.dat probe1y.dat probe2x.dat probe2y.dat probe3x.dat probe4x.dat: probe

clean::
	rm -f probe1x.dat probe1y.dat probe2x.dat probe2y.dat probe3x.dat probe4x.dat

probe: plot.mix.cgns mixing.lay probe.mcr
	$(TECPLOT) -b -p probe.mcr mixing.lay

clean::
	rm -f mixing.lay

plot-lat-1.mix.eps: plot-lat-1.gp solution.pl probe1x.dat probe2x.dat
	cat plot-lat-1.gp | gnuplot > $@

plot-lat-2.mix.eps: plot-lat-2.gp solution.pl  probe3x.dat probe4x.dat
	cat plot-lat-2.gp | gnuplot > $@

plot-long.mix.eps: plot-long.gp solution.pl  probe1y.dat probe2y.dat
	cat plot-long.gp | gnuplot > $@

# # -------------------------------------------------------------
# # no mixing with zero diffusivity
# # -------------------------------------------------------------
# plot.nomix.cgns: $(MODELFILES) $(MODEL) bcspecs.base
# 	sed -e '24s/3.00000/0.00000/' mass2_v027.base > mass2_v027.cfg
# 	cp bcspecs.base bcspecs.dat
# 	$(MODEL)
# 	mv plot000.cgns plot.nomix.cgns

# clean:: 
# 	rm -f plot.nomix.cgns mass2_v027.cfg bcspecs.dat

# nomixing.eps: plot.nomix.cgns mixing.base.lay
# 	sed -e 's/plot\.cgns/plot.nomix.cgns/g' mixing.base.lay > nomixing.lay
# 	$(TECPLOT) -b -y $@ nomixing.lay -p ../../../tecplot/macros/export_eps.mcr

# clean::
# 	rm -f nomixing.lay

# plot-lat-1.nomix.eps: plot-lat-1.gp solution.pl plot.nomix.cgns
# 	sed -e 's/plot\.cgns/plot.nomix.cgns/g' plot-lat-1.gp | gnuplot > $@

# plot-long.nomix.eps: plot-long.gp solution.pl plot.nomix.cgns
# 	sed -e 's/plot\.cgns/plot.nomix.cgns/g' plot-long.gp | gnuplot > $@


# # -------------------------------------------------------------
# # no mixing with wall
# # -------------------------------------------------------------
# plot.wall.cgns: $(MODELFILES) $(MODEL) bcspecs.wall
# 	cp mass2_v027.base mass2_v027.cfg
# 	cp bcspecs.wall bcspecs.dat
# 	$(MODEL)
# 	mv plot000.cgns plot.wall.cgns

# clean:: 
# 	rm -f plot.wall.cgns mass2_v027.cfg bcspecs.dat

# wall.eps: plot.wall.cgns mixing.base.lay
# 	sed -e 's/plot\.cgns/plot.wall.cgns/g' mixing.base.lay > wall.lay
# 	$(TECPLOT) -b -y $@ wall.lay -p ../../../tecplot/macros/export_eps.mcr

# clean:: 
# 	rm -f wall.lay

# plot-lat-1.wall.eps: plot-lat-1.gp solution.pl plot.wall.cgns
# 	sed -e 's/plot\.cgns/plot.wall.cgns/g' plot-lat-1.gp | gnuplot > $@

# plot-long.wall.eps: plot-long.gp solution.pl plot.wall.cgns
# 	sed -e 's/plot\.cgns/plot.wall.cgns/g' plot-long.gp | gnuplot > $@


clean:: 
	rm -f *~
	rm -f error-warning*.out mass_source_monitor.out output*.out status*.out
	rm -f gridplot1.dat
	rm -f plot000.cgns grid.cgns
	rm -f batch.log
	rm -f $(EPSFILES)
