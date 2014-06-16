# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 26, 2003 by William A. Perkins
# Last Change: 2014-06-16 08:01:26 d3g096
# -------------------------------------------------------------

AM_FCFLAGS = \
    $(CGNS_FCFLAGS) \
    $(F90MODFLAG)time_series \
    $(GA_FCFLAGS) $(PETSC_FCFLAGS)

SUBDIRS = time_series . util

TSLIB = time_series/libts.a

bin_PROGRAMS = mass2
check_PROGRAMS = solver_test

mass2_SOURCES = \
	bed.f90 \
	bed_functions.f90 \
	bed_var.f90 \
	block_grid.f90 \
	block_hydro.f90 \
	block_hydro_bc.f90 \
	block_initialization.f90 \
	block_parallel.f90 \
	block_var.f90 \
	block_var_base.f90 \
	differencing.f90 \
	distance.f90 \
	energy_flux_module.f90 \
	gage_output_module.f90 \
	gas_coeffs_module.f90 \
	gas_functions_module.f90 \
	generic_source.f90 \
	globals.f90 \
	hotstart.f90 \
	hydro_bc.f90 \
	hydro_solve.f90 \
	mass2_config.f90 \
	mass2_parallel.f90 \
	mass_source_output.f90 \
	met_data_module.f90 \
	particulate_source.f90 \
	plot_cgns.f90 \
	plot_output.f90 \
	profile_init.f90 \
	realequal.f90 \
	scalar_bc.f90 \
	scalar_solve.f90 \
	scalars_module.f90 \
	scalar_mass_balance.f90 \
	scalars_source.f90 \
	sediment_source.f90 \
	solver_common.f90 \
	table_bc_module.f90 \
	transport_only_module.f90 \
	tdg_source.f90 \
	temperature_source.f90 \
	solver_petsc.F90
mass2_LDADD = \
	$(TSLIB) \
	$(CGNS_LIBS) \
	$(PETSC_FLIBS) \
	$(GA_LIBS)

solver_test_SOURCES = \
	solver_test.f90 \
	solver_common.f90 \
	solver_backup.f90 \
	solver_petsc.F90
solver_test_LDADD = $(TSLIB) \
	$(PETSC_FLIBS) -lm

# dependancies for individual object files

bed.$(OBJEXT): bed.f90 \
	bed_var.$(OBJEXT) \
	block_parallel.$(OBJEXT) \
	scalars_source.$(OBJEXT) \
	scalars_module.$(OBJEXT)

bed_functions.$(OBJEXT): bed_functions.f90 \
	bed.$(OBJEXT)

bed_var.$(OBJEXT): bed_var.f90 \
	block_var_base.$(OBJEXT) \
	$(TSLIB)
block_grid.$(OBJEXT): block_grid.f90 block_parallel.$(OBJEXT) mass2_config.$(OBJEXT) $(TSLIB)
block_hydro.$(OBJEXT): block_hydro.f90 block_parallel.$(OBJEXT) mass2_config.$(OBJEXT)  $(TSLIB)
block_hydro_bc.$(OBJEXT): block_hydro_bc.f90 hydro_bc.$(OBJEXT) block_grid.$(OBJEXT) 
block_initialization.$(OBJEXT): block_initialization.f90 block_hydro.$(OBJEXT) \
	hotstart.$(OBJEXT) scalars_module.$(OBJEXT) profile_init.$(OBJEXT) 
block_parallel.$(OBJEXT): block_parallel.f90 block_var.$(OBJEXT)
block_var.$(OBJEXT): block_var.f90 block_var_base.$(OBJEXT)
block_var_base.$(OBJEXT): block_var_base.f90 globals.$(OBJEXT)
diffenencing.$(OBJEXT): differencing.f90
energy_flux_module.$(OBJEXT): energy_flux_module.f90
gas_coeffs_module.$(OBJEXT): gas_coeffs_module.f90
gas_functions_module.$(OBJEXT): gas_functions_module.f90 gas_coeffs_module.$(OBJEXT)
globals.$(OBJEXT): globals.f90  $(TSLIB)
gage_output_module.$(OBJEXT): gage_output_module.f90 \
	block_parallel.$(OBJEXT) \
	mass2_config.$(OBJEXT) \
	scalars_source.$(OBJEXT)  \
	bed.$(OBJEXT) \
	$(TSLIB)
hotstart.$(OBJEXT): hotstart.f90 \
	block_parallel.$(OBJEXT) \
	mass2_config.$(OBJEXT) \
	scalars_module.$(OBJEXT) \
	bed.$(OBJEXT)
hydro_bc.$(OBJEXT): hydro_bc.f90 table_bc_module.$(OBJEXT) mass2_config.$(OBJEXT) $(TSLIB)
hydro_solve.$(OBJEXT): hydro_solve.f90 block_hydro.$(OBJEXT) block_hydro_bc.$(OBJEXT) differencing.$(OBJEXT) \
	solver_petsc.$(OBJEXT) mass2_config.$(OBJEXT)
mass2_config.$(OBJEXT): mass2_config.f90 differencing.$(OBJEXT) solver_common.$(OBJEXT) block_var_base.$(OBJEXT)  $(TSLIB)

mass_source_output.$(OBJEXT): mass_source_output.f90 \
	mass2_config.$(OBJEXT) \
	globals.$(OBJEXT) \
	block_parallel.$(OBJEXT)
met_data_module.$(OBJEXT): met_data_module.f90
particulate_source.$(OBJEXT): particulate_source.f90 \
	mass2_config.$(OBJEXT) \
	scalars_module.$(OBJEXT) \
	sediment_source.$(OBJEXT) \
	bed_functions.inc \
	$(TSLIB)

plot_cgns.$(OBJEXT): plot_cgns.f90 \
	block_grid.$(OBJEXT) \
	block_hydro.$(OBJEXT) \
	block_hydro_bc.$(OBJEXT) \
	scalars_module.$(OBJEXT) \
	scalars_source.$(OBJEXT)  \
	bed.$(OBJEXT) \
	$(TSLIB)
plot_output.$(OBJEXT): plot_output.f90 plot_cgns.$(OBJEXT) mass2_config.$(OBJEXT) block_parallel.$(OBJEXT)
profile_init.$(OBJEXT): profile_init.f90 mass2_config.$(OBJEXT) block_parallel.$(OBJEXT) $(TSLIB)
realequal.$(OBJEXT): realequal.f90
scalar_bc.$(OBJEXT): scalar_bc.f90 table_bc_module.$(OBJEXT) mass2_config.$(OBJEXT) $(TSLIB)
scalar_solve.$(OBJEXT): scalar_solve.f90 \
	globals.$(OBJEXT) \
	block_hydro.$(OBJEXT) \
	block_hydro_bc.$(OBJEXT) \
	scalars_module.$(OBJEXT) \
	scalars_source.$(OBJEXT) \
	scalar_bc.$(OBJEXT) \
	transport_only_module.$(OBJEXT) \
	differencing.$(OBJEXT) \
	solver_petsc.$(OBJEXT)
scalars_module.$(OBJEXT): scalars_module.f90 \
	globals.$(OBJEXT) \
	block_var.$(OBJEXT) \
	$(TSLIB)
scalar_mass_balance.$(OBJEXT): scalar_mass_balance.f90 \
	globals.$(OBJEXT) \
	scalars_source.$(OBJEXT) \
	bed.$(OBJEXT) \
	$(TSLIB)
scalars_source.$(OBJEXT): scalars_source.f90 \
	generic_source.$(OBJEXT) \
	temperature_source.$(OBJEXT) \
	tdg_source.$(OBJEXT) \
	sediment_source.$(OBJEXT) \
	particulate_source.$(OBJEXT) \
	$(TSLIB)
sediment_source.$(OBJEXT): sediment_source.f90 \
	mass2_config.$(OBJEXT) \
	block_parallel.$(OBJEXT) \
	scalars_module.$(OBJEXT) \
	$(TSLIB)
solver_common.$(OBJEXT): solver_common.f90
solver_petsc.$(OBJEXT): solver_petsc.F90 solver_common.$(OBJEXT)
solver_test.$(OBJEXT): solver_test.f90 solver_common.$(OBJEXT) solver_petsc.$(OBJEXT)
table_bc_module.$(OBJEXT): table_bc_module.f90  $(TSLIB)
tdg_source.$(OBJEXT): tdg_source.f90 \
	gas_functions_module.$(OBJEXT) \
	met_data_module.$(OBJEXT)
temperature_source.$(OBJEXT): temperature_source.f90 \
	met_data_module.$(OBJEXT) \
	energy_flux_module.$(OBJEXT) 
transport_only_module.$(OBJEXT): transport_only_module.f90 \
	block_hydro.$(OBJEXT) \
	hotstart.$(OBJEXT)

mass2_parallel.$(OBJEXT): mass2_parallel.f90 \
	bed.$(OBJEXT) \
	block_grid.$(OBJEXT) \
	block_initialization.$(OBJEXT) \
	mass2_config.$(OBJEXT) \
	mass_source_output.$(OBJEXT) \
	gage_output_module.$(OBJEXT) \
	plot_output.$(OBJEXT) \
	hotstart.$(OBJEXT) \
	scalar_bc.$(OBJEXT) \
	scalar_mass_balance.$(OBJEXT) \
	scalars_module.$(OBJEXT) \
	scalar_solve.$(OBJEXT) \
	solver_petsc.$(OBJEXT) \
	met_data_module.$(OBJEXT) \
	transport_only_module.$(OBJEXT) \
	$(TSLIB)

.f90.o:
	$(FCCOMPILE) $(FC_CPPFLAG) -c -o $@ $<

clean-local:
	rm -f *.$(F90MODEXT)
	rm -f *.il work.*
	rm -f *~

# extra files to distribute

EXTRA_DIST = \
	README \
	Makefile.mpi \
	bed_functions.inc
