# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 26, 2003 by William A. Perkins
# Last Change: Mon Apr 21 14:46:05 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

SUBDIRS = @SUBDIRS@

bin_PROGRAMS = mass2


mass2_SOURCES =									\
     gas_coeffs_module.f90						\
     gas_functions_module.f90					\
     scalars_module.f90							\
     energy_flux_module.f90						\
     gage_output_module.f90						\
     plot_cgns.f90  							\
	 plot_netcdf.f90							\
     plot_output.f90							\
     netcdferror.f90							\
     global_module_023.f90						\
     io_routines_module.f90						\
     table_bc_module.f90						\
     block_bc_module.f90						\
     met_data_module.f90						\
     profile_init.f90							\
     misc_vars_module.f90						\
     transport_only_module.f90					\
     mass2_main_025.f90							\
     mass2.f90									\
	 scalars_source.f90							\
	 temperature_source.f90						\
	 tdg_source.f90								\
	 generic_source.f90							\
	 bed_source.f90								\
	 sediment_source.f90						\
	 particulate_source.f90						\
	 bed.f90									\
	 bed_functions.f90							\
	 accumulator.f90							\
	 total_conc.f90								\
	 solver_tdma.f90 \
	 scalar_mass_balance.f90
mass2_LDADD = time_series/libts.a

# dependancies for individual object files

accumulator.o: accumulator.f90 global_module_023.o scalars_module.o \
	scalars_source.o misc_vars_module.o 

bed.o: bed.f90 scalars_source.o scalars_module.o global_module_023.o \
	misc_vars_module.o

bed_functions.o: bed_functions.f90 bed.o

bed_source.o: bed_source.f90 table_bc_module.o global_module_023.o \
	misc_vars_module.o

block_bc_module.o: block_bc_module.f90 table_bc_module.o

energy_flux_module.o: energy_flux_module.f90

gage_output_module.o: gage_output_module.f90 global_module_023.o \
	scalars_module.o gas_functions_module.o  bed.o

gas_coeffs_module.o: gas_coeffs_module.f90

gas_functions_module.o: gas_functions_module.f90 gas_coeffs_module.o

generic_source.o: generic_source.f90 misc_vars_module.o bed_source.o

global_module_023.o: global_module_023.f90 misc_vars_module.o

io_routines_module.o: io_routines_module.f90

mass2_main_025.o: mass2_main_025.f90 \
	global_module_023.o io_routines_module.o block_bc_module.o table_bc_module.o \
	 scalars_module.o met_data_module.o energy_flux_module.o \
	gas_functions_module.o plot_output.o gage_output_module.o misc_vars_module.o \
	transport_only_module.o scalars_source.o bed.o accumulator.o scalar_mass_balance.o

mass2.o: mass2.f90 mass2_main_025.o

misc_vars_module.o: misc_vars_module.f90 

met_data_module.o: met_data_module.f90 table_bc_module.o 

netcdferror.o: netcdferror.f90

particulate_source.o: particulate_source.f90 \
	global_module_023.o scalars_module.o misc_vars_module.o bed_functions.inc

plot_cgns.o: plot_cgns.f90 global_module_023.o scalars_module.o \
	scalars_source.o bed.o misc_vars_module.o accumulator.o \
	gas_functions_module.o bed.o

plot_netcdf.o: plot_netcdf.f90 global_module_023.o scalars_module.o \
	scalars_source.o bed.o misc_vars_module.o accumulator.o \
	gas_functions_module.o bed.o

plot_output.o: plot_output.f90 global_module_023.o scalars_module.o \
	scalars_source.o bed.o misc_vars_module.o accumulator.o \
	gas_functions_module.o plot_cgns.o plot_netcdf.o

scalar_mass_balance.o: scalar_mass_balance.f90 global_module_023.o \
	scalars_source.o bed.o

scalars_module.o: scalars_module.f90 misc_vars_module.o

scalars_source.o: scalars_source.f90 temperature_source.o \
	tdg_source.o generic_source.o scalars_module.o met_data_module.o \
	sediment_source.o particulate_source.o

sediment_source.o: sediment_source.f90 global_module_023.o scalars_module.o \
	misc_vars_module.o

solver_tdma.o: solver_tdma.f90

table_bc_module.o: table_bc_module.f90 

tdg_source.o: tdg_source.f90 misc_vars_module.o gas_functions_module.o \
	met_data_module.o

temperature_source.o: temperature_source.f90 met_data_module.o \
	misc_vars_module.o energy_flux_module.o

total_conc.o: total_conc.f90 scalars_source.o

transport_only_module.o: transport_only_module.f90  \
	global_module_023.o misc_vars_module.o

profile_init.o: profile_init.f90 global_module_023.o

clean-local:
	rm -f *.$(F90MODULEEXT)
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
	mass2.dsw mass2.dsp mass2.mak \
	bed_functions.inc