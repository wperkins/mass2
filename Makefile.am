# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 26, 2003 by William A. Perkins
# Last Change: Fri May 27 14:59:32 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------

AM_FFLAGS = $(FCFLAGS)

SUBDIRS = @SUBDIRS@

bin_PROGRAMS = mass2
check_PROGRAMS = solver_test

mass2_SOURCES =					\
     constants.f90				\
     gas_coeffs_module.f90			\
     gas_functions_module.f90			\
     scalars_module.f90				\
     energy_flux_module.f90			\
     gage_output_module.f90			\
     plot_cgns.f90				\
	 plot_netcdf.f90			\
     plot_output.f90				\
     netcdferror.f90				\
     global_module_023.f90			\
     io_routines_module.f90			\
     table_bc_module.f90			\
     block_bc_module.f90			\
     met_data_module.f90			\
     profile_init.f90				\
     misc_vars_module.f90			\
     transport_only_module.f90			\
     mass2_main_025.f90				\
     mass2.f90					\
	 scalars_source.f90			\
	 scalar_solve.f90			\
	 temperature_source.f90			\
	 tdg_source.f90				\
	 generic_source.f90			\
	 bed_source.f90				\
	 sediment_source.f90			\
	 particulate_source.f90			\
	 bed.f90				\
	 bed_functions.f90			\
	 accumulator.f90			\
	 total_conc.f90				\
	 hydro_solve.f90			\
	 scalar_mass_balance.f90		\
	 solver_backup.f90			\
	 solver_common.f90			\
	 distance.f90				\
	 differencing.f90                       \
	 block_flux_output.f90
mass2_LDADD = $(SOLVEROBJ) time_series/libts.a \
	@SOLVERLDFLAGS@ \
	-L@NETCDF_LIBS_PATH@ @NETCDF_LIBS@ \
	-L@CGNS_LIBS_PATH@ @CGNS_LIBS@

solver_test_SOURCES =							\
	solver_test.f90								\
	solver_backup.f90
solver_test_DEPENDENCIES = $(SOLVEROBJ)
solver_test_LDADD = $(SOLVEROBJ) time_series/libts.a @SOLVERLDFLAGS@

# dependancies for individual object files

accumulator.$(OBJEXT): accumulator.f90 global_module_023.$(OBJEXT) scalars_module.$(OBJEXT) \
	scalars_source.$(OBJEXT) misc_vars_module.$(OBJEXT) bed.$(OBJEXT)

bed.$(OBJEXT): bed.f90 scalars_source.$(OBJEXT) scalars_module.$(OBJEXT) global_module_023.$(OBJEXT) \
	misc_vars_module.$(OBJEXT) time_series/libts.a

bed_functions.$(OBJEXT): bed_functions.f90 bed.$(OBJEXT)

bed_source.$(OBJEXT): bed_source.f90 table_bc_module.$(OBJEXT) global_module_023.$(OBJEXT) \
	misc_vars_module.$(OBJEXT) time_series/libts.a

block_bc_module.$(OBJEXT): block_bc_module.f90 table_bc_module.$(OBJEXT) global_module_023.$(OBJEXT) \
	time_series/libts.a

block_flux_output.$(OBJEXT): block_flux_output.f90 global_module_023.$(OBJEXT) 

diffenencing.$(OBJEXT): differencing.f90

energy_flux_module.$(OBJEXT): energy_flux_module.f90 constants.$(OBJEXT)

gage_output_module.$(OBJEXT): gage_output_module.f90 global_module_023.$(OBJEXT) \
	scalars_module.$(OBJEXT) gas_functions_module.$(OBJEXT)  bed.$(OBJEXT) time_series/libts.a

gas_coeffs_module.$(OBJEXT): gas_coeffs_module.f90

gas_functions_module.$(OBJEXT): gas_functions_module.f90 gas_coeffs_module.$(OBJEXT)

generic_source.$(OBJEXT): generic_source.f90 misc_vars_module.$(OBJEXT) bed_source.$(OBJEXT) constants.$(OBJEXT)

global_module_023.$(OBJEXT): global_module_023.f90 misc_vars_module.$(OBJEXT) time_series/libts.a

hydro_solve.$(OBJEXT): hydro_solve.f90  global_module_023.f90 misc_vars_module.$(OBJEXT)\
	 differencing.$(OBJEXT) $(SOLVEROBJ)

io_routines_module.$(OBJEXT): io_routines_module.f90

mass2_main_025.$(OBJEXT): mass2_main_025.f90 \
	global_module_023.$(OBJEXT) io_routines_module.$(OBJEXT) block_bc_module.$(OBJEXT) table_bc_module.$(OBJEXT) \
	 scalars_module.$(OBJEXT) met_data_module.$(OBJEXT) energy_flux_module.$(OBJEXT) \
	gas_functions_module.$(OBJEXT) plot_output.$(OBJEXT) gage_output_module.$(OBJEXT) misc_vars_module.$(OBJEXT) \
	transport_only_module.$(OBJEXT) scalars_source.$(OBJEXT) bed.$(OBJEXT) accumulator.$(OBJEXT) scalar_mass_balance.$(OBJEXT) \
	differencing.$(OBJEXT) block_flux_output.$(OBJEXT) $(SOLVEROBJ)

mass2.$(OBJEXT): mass2.f90 mass2_main_025.$(OBJEXT)

misc_vars_module.$(OBJEXT): misc_vars_module.f90 

met_data_module.$(OBJEXT): met_data_module.f90 table_bc_module.$(OBJEXT) 

netcdferror.$(OBJEXT): netcdferror.f90

particulate_source.$(OBJEXT): particulate_source.f90 \
	global_module_023.$(OBJEXT) scalars_module.$(OBJEXT) misc_vars_module.$(OBJEXT) bed_functions.inc \
	time_series/libts.a

plot_cgns.$(OBJEXT): plot_cgns.f90 global_module_023.$(OBJEXT) scalars_module.$(OBJEXT) \
	scalars_source.$(OBJEXT) bed.$(OBJEXT) misc_vars_module.$(OBJEXT) accumulator.$(OBJEXT) \
	gas_functions_module.$(OBJEXT) bed.$(OBJEXT) block_bc_module.$(OBJEXT) time_series/libts.a

plot_netcdf.$(OBJEXT): plot_netcdf.f90 global_module_023.$(OBJEXT) scalars_module.$(OBJEXT) \
	scalars_source.$(OBJEXT) bed.$(OBJEXT) misc_vars_module.$(OBJEXT) accumulator.$(OBJEXT) \
	gas_functions_module.$(OBJEXT) bed.$(OBJEXT)

plot_output.$(OBJEXT): plot_output.f90 global_module_023.$(OBJEXT) scalars_module.$(OBJEXT) \
	scalars_source.$(OBJEXT) bed.$(OBJEXT) misc_vars_module.$(OBJEXT) accumulator.$(OBJEXT) \
	gas_functions_module.$(OBJEXT) plot_cgns.$(OBJEXT) plot_netcdf.$(OBJEXT)

profile_init.$(OBJEXT): profile_init.f90 global_module_023.$(OBJEXT) misc_vars_module.$(OBJEXT) \
	time_series/libts.a

scalar_mass_balance.$(OBJEXT): scalar_mass_balance.f90 global_module_023.$(OBJEXT) \
	scalars_source.$(OBJEXT) bed.$(OBJEXT) time_series/libts.a

scalar_solve.$(OBJEXT): scalar_solve.f90 global_module_023.$(OBJEXT) misc_vars_module.$(OBJEXT) \
	scalars_module.$(OBJEXT) $(SOLVEROBJ) differencing.$(OBJEXT)

scalars_module.$(OBJEXT): scalars_module.f90 misc_vars_module.$(OBJEXT) time_series/libts.a

scalars_source.$(OBJEXT): scalars_source.f90 temperature_source.$(OBJEXT) misc_vars_module.$(OBJEXT) \
	tdg_source.$(OBJEXT) generic_source.$(OBJEXT) scalars_module.$(OBJEXT) met_data_module.$(OBJEXT) \
	sediment_source.$(OBJEXT) particulate_source.$(OBJEXT) differencing.$(OBJEXT) time_series/libts.a 

sediment_source.$(OBJEXT): sediment_source.f90 global_module_023.$(OBJEXT) scalars_module.$(OBJEXT) \
	misc_vars_module.$(OBJEXT) time_series/libts.a

solver_aztec.$(OBJEXT): solver_aztec.f90 solver_common.$(OBJEXT)

solver_common.$(OBJEXT): solver_common.f90

solver_sixpack.$(OBJEXT): solver_sixpack.f90 solver_common.$(OBJEXT)

solver_tdma.$(OBJEXT): solver_tdma.f90 solver_common.$(OBJEXT)

solver_test.$(OBJEXT): solver_test.f90 $(SOLVEROBJ)

table_bc_module.$(OBJEXT): table_bc_module.f90  time_series/libts.a

tdg_source.$(OBJEXT): tdg_source.f90 misc_vars_module.$(OBJEXT) gas_functions_module.$(OBJEXT) \
	met_data_module.$(OBJEXT) time_series/libts.a

temperature_source.$(OBJEXT): temperature_source.f90 met_data_module.$(OBJEXT) \
	misc_vars_module.$(OBJEXT) energy_flux_module.$(OBJEXT) global_module_023.$(OBJEXT) \
	time_series/libts.a

total_conc.$(OBJEXT): total_conc.f90 scalars_source.$(OBJEXT)

transport_only_module.$(OBJEXT): transport_only_module.f90  \
	global_module_023.$(OBJEXT) misc_vars_module.$(OBJEXT) time_series/libts.a

clean-local:
	rm -f *.$(F90MODULEEXT)
	rm -f *.il work.*
	rm -f *~

mass2.ps: $(SRCS)
	a2ps --prologue=color -C -R --columns=1 --medium=Letter --font-size=8 --sides=duplex -o $@ $(sort $(mass2_SOURCES))

# extra files to distribute

EXTRA_DIST = \
	README \
	makefile.sgi \
	makefile.linux \
	makefile.intel \
	makefile.dec \
	makefile.lahey \
	Makefile.mpi \
	Makefile.intel.petsc \
	Makefile.petsc \
	mass2.dsw mass2.dsp mass2.mak \
	bed_functions.inc \
	solver_aztec.f90 \
	solver_tdma.f90 \
	solver_sixpack.f90 \
	solver_backup.f90 \
	global_parallel.f90 \
	mass2_parallel.f90
