# Microsoft Developer Studio Generated NMAKE File, Based on mass2.dsp
!IF "$(CFG)" == ""
CFG=mass2_v025 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to mass2_v025 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "mass2_v025 - Win32 Release" && "$(CFG)" !=\
 "mass2_v025 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mass2.mak" CFG="mass2_v025 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mass2_v025 - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "mass2_v025 - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mass2_v027.exe"

!ELSE 

ALL : "$(OUTDIR)\mass2_v027.exe"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\accumulator.mod"
	-@erase "$(INTDIR)\accumulator.obj"
	-@erase "$(INTDIR)\bed.obj"
	-@erase "$(INTDIR)\bed_functions.obj"
	-@erase "$(INTDIR)\bed_module.mod"
	-@erase "$(INTDIR)\bed_source.mod"
	-@erase "$(INTDIR)\bed_source.obj"
	-@erase "$(INTDIR)\block_bc_module.obj"
	-@erase "$(INTDIR)\block_boundary_conditions.mod"
	-@erase "$(INTDIR)\date_time.mod"
	-@erase "$(INTDIR)\date_time_module.obj"
	-@erase "$(INTDIR)\energy_flux.mod"
	-@erase "$(INTDIR)\energy_flux_module.obj"
	-@erase "$(INTDIR)\gage_output.mod"
	-@erase "$(INTDIR)\gage_output_module.obj"
	-@erase "$(INTDIR)\gas_coeffs.mod"
	-@erase "$(INTDIR)\gas_coeffs_module.obj"
	-@erase "$(INTDIR)\gas_functions.mod"
	-@erase "$(INTDIR)\gas_functions_module.obj"
	-@erase "$(INTDIR)\generic_source.mod"
	-@erase "$(INTDIR)\generic_source.obj"
	-@erase "$(INTDIR)\global_module_023.obj"
	-@erase "$(INTDIR)\globals.mod"
	-@erase "$(INTDIR)\io_routines_module.mod"
	-@erase "$(INTDIR)\io_routines_module.obj"
	-@erase "$(INTDIR)\julian.mod"
	-@erase "$(INTDIR)\julian.obj"
	-@erase "$(INTDIR)\mass2.obj"
	-@erase "$(INTDIR)\mass2_main_025.mod"
	-@erase "$(INTDIR)\mass2_main_025.obj"
	-@erase "$(INTDIR)\met_data_module.mod"
	-@erase "$(INTDIR)\met_data_module.obj"
	-@erase "$(INTDIR)\misc_vars.mod"
	-@erase "$(INTDIR)\misc_vars_module.obj"
	-@erase "$(INTDIR)\netcdferror.obj"
	-@erase "$(INTDIR)\particulate_source.mod"
	-@erase "$(INTDIR)\particulate_source.obj"
	-@erase "$(INTDIR)\plot_output.mod"
	-@erase "$(INTDIR)\plot_output.obj"
	-@erase "$(INTDIR)\profile_init.obj"
	-@erase "$(INTDIR)\scalars.mod"
	-@erase "$(INTDIR)\scalars_module.obj"
	-@erase "$(INTDIR)\scalars_source.mod"
	-@erase "$(INTDIR)\scalars_source.obj"
	-@erase "$(INTDIR)\sediment_source.mod"
	-@erase "$(INTDIR)\sediment_source.obj"
	-@erase "$(INTDIR)\table_bc_module.obj"
	-@erase "$(INTDIR)\table_boundary_conditions.mod"
	-@erase "$(INTDIR)\tdg_source.mod"
	-@erase "$(INTDIR)\tdg_source.obj"
	-@erase "$(INTDIR)\temperature_source.mod"
	-@erase "$(INTDIR)\temperature_source.obj"
	-@erase "$(INTDIR)\total_conc.obj"
	-@erase "$(INTDIR)\transport_only.mod"
	-@erase "$(INTDIR)\transport_only_module.obj"
	-@erase "$(OUTDIR)\mass2_v027.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /include:"E:\Software\NetCDF\include"\
 /compile_only /nologo /warn:nofileopt /module:"Release/" /object:"Release/" 
F90_OBJS=.\Release/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mass2.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=netcdfs.lib kernel32.lib /nologo /subsystem:console\
 /incremental:no /pdb:"$(OUTDIR)\mass2_v027.pdb" /machine:I386\
 /nodefaultlib:"libcmt.lib" /out:"$(OUTDIR)\mass2_v027.exe"\
 /libpath:"E:\Software\NetCDF\lib" 
LINK32_OBJS= \
	"$(INTDIR)\accumulator.obj" \
	"$(INTDIR)\bed.obj" \
	"$(INTDIR)\bed_functions.obj" \
	"$(INTDIR)\bed_source.obj" \
	"$(INTDIR)\block_bc_module.obj" \
	"$(INTDIR)\date_time_module.obj" \
	"$(INTDIR)\energy_flux_module.obj" \
	"$(INTDIR)\gage_output_module.obj" \
	"$(INTDIR)\gas_coeffs_module.obj" \
	"$(INTDIR)\gas_functions_module.obj" \
	"$(INTDIR)\generic_source.obj" \
	"$(INTDIR)\global_module_023.obj" \
	"$(INTDIR)\io_routines_module.obj" \
	"$(INTDIR)\julian.obj" \
	"$(INTDIR)\mass2.obj" \
	"$(INTDIR)\mass2_main_025.obj" \
	"$(INTDIR)\met_data_module.obj" \
	"$(INTDIR)\misc_vars_module.obj" \
	"$(INTDIR)\netcdferror.obj" \
	"$(INTDIR)\particulate_source.obj" \
	"$(INTDIR)\plot_output.obj" \
	"$(INTDIR)\profile_init.obj" \
	"$(INTDIR)\scalars_module.obj" \
	"$(INTDIR)\scalars_source.obj" \
	"$(INTDIR)\sediment_source.obj" \
	"$(INTDIR)\table_bc_module.obj" \
	"$(INTDIR)\tdg_source.obj" \
	"$(INTDIR)\temperature_source.obj" \
	"$(INTDIR)\total_conc.obj" \
	"$(INTDIR)\transport_only_module.obj"

"$(OUTDIR)\mass2_v027.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mass2_v027.exe" "$(OUTDIR)\DF50.PDB"

!ELSE 

ALL : "$(OUTDIR)\mass2_v027.exe" "$(OUTDIR)\DF50.PDB"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\accumulator.mod"
	-@erase "$(INTDIR)\accumulator.obj"
	-@erase "$(INTDIR)\bed.obj"
	-@erase "$(INTDIR)\bed_functions.obj"
	-@erase "$(INTDIR)\bed_module.mod"
	-@erase "$(INTDIR)\bed_source.mod"
	-@erase "$(INTDIR)\bed_source.obj"
	-@erase "$(INTDIR)\block_bc_module.obj"
	-@erase "$(INTDIR)\block_boundary_conditions.mod"
	-@erase "$(INTDIR)\date_time.mod"
	-@erase "$(INTDIR)\date_time_module.obj"
	-@erase "$(INTDIR)\DF50.PDB"
	-@erase "$(INTDIR)\energy_flux.mod"
	-@erase "$(INTDIR)\energy_flux_module.obj"
	-@erase "$(INTDIR)\gage_output.mod"
	-@erase "$(INTDIR)\gage_output_module.obj"
	-@erase "$(INTDIR)\gas_coeffs.mod"
	-@erase "$(INTDIR)\gas_coeffs_module.obj"
	-@erase "$(INTDIR)\gas_functions.mod"
	-@erase "$(INTDIR)\gas_functions_module.obj"
	-@erase "$(INTDIR)\generic_source.mod"
	-@erase "$(INTDIR)\generic_source.obj"
	-@erase "$(INTDIR)\global_module_023.obj"
	-@erase "$(INTDIR)\globals.mod"
	-@erase "$(INTDIR)\io_routines_module.mod"
	-@erase "$(INTDIR)\io_routines_module.obj"
	-@erase "$(INTDIR)\julian.mod"
	-@erase "$(INTDIR)\julian.obj"
	-@erase "$(INTDIR)\mass2.obj"
	-@erase "$(INTDIR)\mass2_main_025.mod"
	-@erase "$(INTDIR)\mass2_main_025.obj"
	-@erase "$(INTDIR)\met_data_module.mod"
	-@erase "$(INTDIR)\met_data_module.obj"
	-@erase "$(INTDIR)\misc_vars.mod"
	-@erase "$(INTDIR)\misc_vars_module.obj"
	-@erase "$(INTDIR)\netcdferror.obj"
	-@erase "$(INTDIR)\particulate_source.mod"
	-@erase "$(INTDIR)\particulate_source.obj"
	-@erase "$(INTDIR)\plot_output.mod"
	-@erase "$(INTDIR)\plot_output.obj"
	-@erase "$(INTDIR)\profile_init.obj"
	-@erase "$(INTDIR)\scalars.mod"
	-@erase "$(INTDIR)\scalars_module.obj"
	-@erase "$(INTDIR)\scalars_source.mod"
	-@erase "$(INTDIR)\scalars_source.obj"
	-@erase "$(INTDIR)\sediment_source.mod"
	-@erase "$(INTDIR)\sediment_source.obj"
	-@erase "$(INTDIR)\table_bc_module.obj"
	-@erase "$(INTDIR)\table_boundary_conditions.mod"
	-@erase "$(INTDIR)\tdg_source.mod"
	-@erase "$(INTDIR)\tdg_source.obj"
	-@erase "$(INTDIR)\temperature_source.mod"
	-@erase "$(INTDIR)\temperature_source.obj"
	-@erase "$(INTDIR)\total_conc.obj"
	-@erase "$(INTDIR)\transport_only.mod"
	-@erase "$(INTDIR)\transport_only_module.obj"
	-@erase "$(OUTDIR)\mass2_v027.exe"
	-@erase "$(OUTDIR)\mass2_v027.ilk"
	-@erase "$(OUTDIR)\mass2_v027.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /include:"E:\Software\NetCDF\include"\
 /compile_only /nologo /debug:full /optimize:0 /warn:nofileopt /module:"Debug/"\
 /object:"Debug/" /pdbfile:"Debug/DF50.PDB" 
F90_OBJS=.\Debug/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mass2.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=netcdfs.lib kernel32.lib /nologo /subsystem:console\
 /incremental:yes /pdb:"$(OUTDIR)\mass2_v027.pdb" /debug /machine:I386\
 /nodefaultlib:"libcmt.lib" /out:"$(OUTDIR)\mass2_v027.exe" /pdbtype:sept\
 /libpath:"E:\Software\NetCDF\lib" 
LINK32_OBJS= \
	"$(INTDIR)\accumulator.obj" \
	"$(INTDIR)\bed.obj" \
	"$(INTDIR)\bed_functions.obj" \
	"$(INTDIR)\bed_source.obj" \
	"$(INTDIR)\block_bc_module.obj" \
	"$(INTDIR)\date_time_module.obj" \
	"$(INTDIR)\energy_flux_module.obj" \
	"$(INTDIR)\gage_output_module.obj" \
	"$(INTDIR)\gas_coeffs_module.obj" \
	"$(INTDIR)\gas_functions_module.obj" \
	"$(INTDIR)\generic_source.obj" \
	"$(INTDIR)\global_module_023.obj" \
	"$(INTDIR)\io_routines_module.obj" \
	"$(INTDIR)\julian.obj" \
	"$(INTDIR)\mass2.obj" \
	"$(INTDIR)\mass2_main_025.obj" \
	"$(INTDIR)\met_data_module.obj" \
	"$(INTDIR)\misc_vars_module.obj" \
	"$(INTDIR)\netcdferror.obj" \
	"$(INTDIR)\particulate_source.obj" \
	"$(INTDIR)\plot_output.obj" \
	"$(INTDIR)\profile_init.obj" \
	"$(INTDIR)\scalars_module.obj" \
	"$(INTDIR)\scalars_source.obj" \
	"$(INTDIR)\sediment_source.obj" \
	"$(INTDIR)\table_bc_module.obj" \
	"$(INTDIR)\tdg_source.obj" \
	"$(INTDIR)\temperature_source.obj" \
	"$(INTDIR)\total_conc.obj" \
	"$(INTDIR)\transport_only_module.obj"

"$(OUTDIR)\mass2_v027.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  


!IF "$(CFG)" == "mass2_v025 - Win32 Release" || "$(CFG)" ==\
 "mass2_v025 - Win32 Debug"
SOURCE=.\accumulator.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_ACCUM=\
	".\Release\bed_module.mod"\
	".\Release\date_time.mod"\
	".\Release\gas_functions.mod"\
	".\Release\globals.mod"\
	".\Release\met_data_module.mod"\
	".\Release\misc_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\scalars_source.mod"\
	
F90_MODOUT=\
	"accumulator"


"$(INTDIR)\accumulator.obj"	"$(INTDIR)\accumulator.mod" : $(SOURCE)\
 $(DEP_F90_ACCUM) "$(INTDIR)" "$(INTDIR)\bed_module.mod"\
 "$(INTDIR)\date_time.mod" "$(INTDIR)\gas_functions.mod" "$(INTDIR)\globals.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\scalars_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_ACCUM=\
	".\Debug\bed_module.mod"\
	".\Debug\date_time.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\globals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\scalars_source.mod"\
	
F90_MODOUT=\
	"accumulator"


"$(INTDIR)\accumulator.obj"	"$(INTDIR)\accumulator.mod" : $(SOURCE)\
 $(DEP_F90_ACCUM) "$(INTDIR)" "$(INTDIR)\bed_module.mod"\
 "$(INTDIR)\date_time.mod" "$(INTDIR)\gas_functions.mod" "$(INTDIR)\globals.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\scalars_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\bed.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_BED_F=\
	".\bed_functions.inc"\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\scalars_source.mod"\
	
F90_MODOUT=\
	"bed_module"


"$(INTDIR)\bed.obj"	"$(INTDIR)\bed_module.mod" : $(SOURCE) $(DEP_F90_BED_F)\
 "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\scalars_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_BED_F=\
	".\bed_functions.inc"\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\scalars_source.mod"\
	
F90_MODOUT=\
	"bed_module"


"$(INTDIR)\bed.obj"	"$(INTDIR)\bed_module.mod" : $(SOURCE) $(DEP_F90_BED_F)\
 "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\scalars_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\bed_functions.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_BED_FU=\
	".\Release\bed_module.mod"\
	".\Release\scalars_source.mod"\
	

"$(INTDIR)\bed_functions.obj" : $(SOURCE) $(DEP_F90_BED_FU) "$(INTDIR)"\
 "$(INTDIR)\bed_module.mod" "$(INTDIR)\scalars_source.mod"


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_BED_FU=\
	".\Debug\bed_module.mod"\
	".\Debug\scalars_source.mod"\
	

"$(INTDIR)\bed_functions.obj" : $(SOURCE) $(DEP_F90_BED_FU) "$(INTDIR)"\
 "$(INTDIR)\bed_module.mod" "$(INTDIR)\scalars_source.mod"


!ENDIF 

SOURCE=.\bed_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_BED_S=\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	".\Release\table_boundary_conditions.mod"\
	
F90_MODOUT=\
	"bed_source"


"$(INTDIR)\bed_source.obj"	"$(INTDIR)\bed_source.mod" : $(SOURCE)\
 $(DEP_F90_BED_S) "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\table_boundary_conditions.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_BED_S=\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\table_boundary_conditions.mod"\
	
F90_MODOUT=\
	"bed_source"


"$(INTDIR)\bed_source.obj"	"$(INTDIR)\bed_source.mod" : $(SOURCE)\
 $(DEP_F90_BED_S) "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\table_boundary_conditions.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\block_bc_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_BLOCK=\
	".\Release\table_boundary_conditions.mod"\
	
F90_MODOUT=\
	"block_boundary_conditions"


"$(INTDIR)\block_bc_module.obj"	"$(INTDIR)\block_boundary_conditions.mod" : \
$(SOURCE) $(DEP_F90_BLOCK) "$(INTDIR)"\
 "$(INTDIR)\table_boundary_conditions.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_BLOCK=\
	".\Debug\table_boundary_conditions.mod"\
	
F90_MODOUT=\
	"block_boundary_conditions"


"$(INTDIR)\block_bc_module.obj"	"$(INTDIR)\block_boundary_conditions.mod" : \
$(SOURCE) $(DEP_F90_BLOCK) "$(INTDIR)"\
 "$(INTDIR)\table_boundary_conditions.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\date_time_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_DATE_=\
	".\Release\julian.mod"\
	
F90_MODOUT=\
	"date_time"


"$(INTDIR)\date_time_module.obj"	"$(INTDIR)\date_time.mod" : $(SOURCE)\
 $(DEP_F90_DATE_) "$(INTDIR)" "$(INTDIR)\julian.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_DATE_=\
	".\Debug\julian.mod"\
	
F90_MODOUT=\
	"date_time"


"$(INTDIR)\date_time_module.obj"	"$(INTDIR)\date_time.mod" : $(SOURCE)\
 $(DEP_F90_DATE_) "$(INTDIR)" "$(INTDIR)\julian.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\energy_flux_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

F90_MODOUT=\
	"energy_flux"


"$(INTDIR)\energy_flux_module.obj"	"$(INTDIR)\energy_flux.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

F90_MODOUT=\
	"energy_flux"


"$(INTDIR)\energy_flux_module.obj"	"$(INTDIR)\energy_flux.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\gage_output_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_GAGE_=\
	".\Release\bed_module.mod"\
	".\Release\date_time.mod"\
	".\Release\gas_functions.mod"\
	".\Release\globals.mod"\
	".\Release\scalars.mod"\
	".\Release\scalars_source.mod"\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
F90_MODOUT=\
	"gage_output"


"$(INTDIR)\gage_output_module.obj"	"$(INTDIR)\gage_output.mod" : $(SOURCE)\
 $(DEP_F90_GAGE_) "$(INTDIR)" "$(INTDIR)\bed_module.mod"\
 "$(INTDIR)\date_time.mod" "$(INTDIR)\gas_functions.mod" "$(INTDIR)\globals.mod"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\scalars_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_GAGE_=\
	".\Debug\bed_module.mod"\
	".\Debug\date_time.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\globals.mod"\
	".\Debug\scalars.mod"\
	".\Debug\scalars_source.mod"\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
F90_MODOUT=\
	"gage_output"


"$(INTDIR)\gage_output_module.obj"	"$(INTDIR)\gage_output.mod" : $(SOURCE)\
 $(DEP_F90_GAGE_) "$(INTDIR)" "$(INTDIR)\bed_module.mod"\
 "$(INTDIR)\date_time.mod" "$(INTDIR)\gas_functions.mod" "$(INTDIR)\globals.mod"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\scalars_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\gas_coeffs_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

F90_MODOUT=\
	"gas_coeffs"


"$(INTDIR)\gas_coeffs_module.obj"	"$(INTDIR)\gas_coeffs.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

F90_MODOUT=\
	"gas_coeffs"


"$(INTDIR)\gas_coeffs_module.obj"	"$(INTDIR)\gas_coeffs.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\gas_functions_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_GAS_F=\
	".\Release\gas_coeffs.mod"\
	
F90_MODOUT=\
	"gas_functions"


"$(INTDIR)\gas_functions_module.obj"	"$(INTDIR)\gas_functions.mod" : $(SOURCE)\
 $(DEP_F90_GAS_F) "$(INTDIR)" "$(INTDIR)\gas_coeffs.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_GAS_F=\
	".\Debug\gas_coeffs.mod"\
	
F90_MODOUT=\
	"gas_functions"


"$(INTDIR)\gas_functions_module.obj"	"$(INTDIR)\gas_functions.mod" : $(SOURCE)\
 $(DEP_F90_GAS_F) "$(INTDIR)" "$(INTDIR)\gas_coeffs.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\generic_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_GENER=\
	".\bed_functions.inc"\
	".\Release\bed_source.mod"\
	".\Release\misc_vars.mod"\
	
F90_MODOUT=\
	"generic_source"


"$(INTDIR)\generic_source.obj"	"$(INTDIR)\generic_source.mod" : $(SOURCE)\
 $(DEP_F90_GENER) "$(INTDIR)" "$(INTDIR)\bed_source.mod"\
 "$(INTDIR)\misc_vars.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_GENER=\
	".\bed_functions.inc"\
	".\Debug\bed_source.mod"\
	".\Debug\misc_vars.mod"\
	
F90_MODOUT=\
	"generic_source"


"$(INTDIR)\generic_source.obj"	"$(INTDIR)\generic_source.mod" : $(SOURCE)\
 $(DEP_F90_GENER) "$(INTDIR)" "$(INTDIR)\bed_source.mod"\
 "$(INTDIR)\misc_vars.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\global_module_023.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

F90_MODOUT=\
	"globals"


"$(INTDIR)\global_module_023.obj"	"$(INTDIR)\globals.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

F90_MODOUT=\
	"globals"


"$(INTDIR)\global_module_023.obj"	"$(INTDIR)\globals.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\io_routines_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

F90_MODOUT=\
	"io_routines_module"


"$(INTDIR)\io_routines_module.obj"	"$(INTDIR)\io_routines_module.mod" : \
$(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

F90_MODOUT=\
	"io_routines_module"


"$(INTDIR)\io_routines_module.obj"	"$(INTDIR)\io_routines_module.mod" : \
$(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\julian.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

F90_MODOUT=\
	"julian"


"$(INTDIR)\julian.obj"	"$(INTDIR)\julian.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

F90_MODOUT=\
	"julian"


"$(INTDIR)\julian.obj"	"$(INTDIR)\julian.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\mass2.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_MASS2=\
	".\Release\mass2_main_025.mod"\
	".\Release\misc_vars.mod"\
	

"$(INTDIR)\mass2.obj" : $(SOURCE) $(DEP_F90_MASS2) "$(INTDIR)"\
 "$(INTDIR)\mass2_main_025.mod" "$(INTDIR)\misc_vars.mod"


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_MASS2=\
	".\Debug\mass2_main_025.mod"\
	".\Debug\misc_vars.mod"\
	

"$(INTDIR)\mass2.obj" : $(SOURCE) $(DEP_F90_MASS2) "$(INTDIR)"\
 "$(INTDIR)\mass2_main_025.mod" "$(INTDIR)\misc_vars.mod"


!ENDIF 

SOURCE=.\mass2_main_025.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_MASS2_=\
	".\Release\bed_module.mod"\
	".\Release\block_boundary_conditions.mod"\
	".\Release\date_time.mod"\
	".\Release\energy_flux.mod"\
	".\Release\gage_output.mod"\
	".\Release\gas_functions.mod"\
	".\Release\globals.mod"\
	".\Release\io_routines_module.mod"\
	".\Release\met_data_module.mod"\
	".\Release\misc_vars.mod"\
	".\Release\plot_output.mod"\
	".\Release\scalars.mod"\
	".\Release\scalars_source.mod"\
	".\Release\table_boundary_conditions.mod"\
	".\Release\transport_only.mod"\
	
F90_MODOUT=\
	"mass2_main_025"


"$(INTDIR)\mass2_main_025.obj"	"$(INTDIR)\mass2_main_025.mod" : $(SOURCE)\
 $(DEP_F90_MASS2_) "$(INTDIR)" "$(INTDIR)\globals.mod"\
 "$(INTDIR)\io_routines_module.mod" "$(INTDIR)\block_boundary_conditions.mod"\
 "$(INTDIR)\date_time.mod" "$(INTDIR)\gage_output.mod"\
 "$(INTDIR)\energy_flux.mod" "$(INTDIR)\gas_functions.mod"\
 "$(INTDIR)\bed_module.mod" "$(INTDIR)\met_data_module.mod"\
 "$(INTDIR)\misc_vars.mod" "$(INTDIR)\plot_output.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\scalars_source.mod" "$(INTDIR)\table_boundary_conditions.mod"\
 "$(INTDIR)\transport_only.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_MASS2_=\
	".\Debug\bed_module.mod"\
	".\Debug\block_boundary_conditions.mod"\
	".\Debug\date_time.mod"\
	".\Debug\energy_flux.mod"\
	".\Debug\gage_output.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\globals.mod"\
	".\Debug\io_routines_module.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\plot_output.mod"\
	".\Debug\scalars.mod"\
	".\Debug\scalars_source.mod"\
	".\Debug\table_boundary_conditions.mod"\
	".\Debug\transport_only.mod"\
	
F90_MODOUT=\
	"mass2_main_025"


"$(INTDIR)\mass2_main_025.obj"	"$(INTDIR)\mass2_main_025.mod" : $(SOURCE)\
 $(DEP_F90_MASS2_) "$(INTDIR)" "$(INTDIR)\globals.mod"\
 "$(INTDIR)\io_routines_module.mod" "$(INTDIR)\block_boundary_conditions.mod"\
 "$(INTDIR)\date_time.mod" "$(INTDIR)\gage_output.mod"\
 "$(INTDIR)\energy_flux.mod" "$(INTDIR)\gas_functions.mod"\
 "$(INTDIR)\bed_module.mod" "$(INTDIR)\met_data_module.mod"\
 "$(INTDIR)\misc_vars.mod" "$(INTDIR)\plot_output.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\scalars_source.mod" "$(INTDIR)\table_boundary_conditions.mod"\
 "$(INTDIR)\transport_only.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\met_data_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_MET_D=\
	".\Release\date_time.mod"\
	".\Release\table_boundary_conditions.mod"\
	
F90_MODOUT=\
	"met_data_module"


"$(INTDIR)\met_data_module.obj"	"$(INTDIR)\met_data_module.mod" : $(SOURCE)\
 $(DEP_F90_MET_D) "$(INTDIR)" "$(INTDIR)\date_time.mod"\
 "$(INTDIR)\table_boundary_conditions.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_MET_D=\
	".\Debug\date_time.mod"\
	".\Debug\table_boundary_conditions.mod"\
	
F90_MODOUT=\
	"met_data_module"


"$(INTDIR)\met_data_module.obj"	"$(INTDIR)\met_data_module.mod" : $(SOURCE)\
 $(DEP_F90_MET_D) "$(INTDIR)" "$(INTDIR)\date_time.mod"\
 "$(INTDIR)\table_boundary_conditions.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\misc_vars_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_MISC_=\
	".\Release\date_time.mod"\
	
F90_MODOUT=\
	"misc_vars"


"$(INTDIR)\misc_vars_module.obj"	"$(INTDIR)\misc_vars.mod" : $(SOURCE)\
 $(DEP_F90_MISC_) "$(INTDIR)" "$(INTDIR)\date_time.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_MISC_=\
	".\Debug\date_time.mod"\
	
F90_MODOUT=\
	"misc_vars"


"$(INTDIR)\misc_vars_module.obj"	"$(INTDIR)\misc_vars.mod" : $(SOURCE)\
 $(DEP_F90_MISC_) "$(INTDIR)" "$(INTDIR)\date_time.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\netcdferror.f90
DEP_F90_NETCD=\
	"E:\Software\NetCDF\include\netcdf.inc"\
	

"$(INTDIR)\netcdferror.obj" : $(SOURCE) $(DEP_F90_NETCD) "$(INTDIR)"


SOURCE=.\particulate_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_PARTI=\
	".\bed_functions.inc"\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\sediment_source.mod"\
	
F90_MODOUT=\
	"particulate_source"


"$(INTDIR)\particulate_source.obj"	"$(INTDIR)\particulate_source.mod" : \
$(SOURCE) $(DEP_F90_PARTI) "$(INTDIR)" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\globals.mod" "$(INTDIR)\scalars.mod" "$(INTDIR)\sediment_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_PARTI=\
	".\bed_functions.inc"\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\sediment_source.mod"\
	
F90_MODOUT=\
	"particulate_source"


"$(INTDIR)\particulate_source.obj"	"$(INTDIR)\particulate_source.mod" : \
$(SOURCE) $(DEP_F90_PARTI) "$(INTDIR)" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\globals.mod" "$(INTDIR)\scalars.mod" "$(INTDIR)\sediment_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\plot_output.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_PLOT_=\
	".\Release\accumulator.mod"\
	".\Release\bed_module.mod"\
	".\Release\date_time.mod"\
	".\Release\gas_functions.mod"\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\scalars_source.mod"\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
F90_MODOUT=\
	"plot_output"


"$(INTDIR)\plot_output.obj"	"$(INTDIR)\plot_output.mod" : $(SOURCE)\
 $(DEP_F90_PLOT_) "$(INTDIR)" "$(INTDIR)\accumulator.mod"\
 "$(INTDIR)\misc_vars.mod" "$(INTDIR)\globals.mod" "$(INTDIR)\gas_functions.mod"\
 "$(INTDIR)\date_time.mod" "$(INTDIR)\bed_module.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\scalars_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_PLOT_=\
	".\Debug\accumulator.mod"\
	".\Debug\bed_module.mod"\
	".\Debug\date_time.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\scalars_source.mod"\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
F90_MODOUT=\
	"plot_output"


"$(INTDIR)\plot_output.obj"	"$(INTDIR)\plot_output.mod" : $(SOURCE)\
 $(DEP_F90_PLOT_) "$(INTDIR)" "$(INTDIR)\accumulator.mod"\
 "$(INTDIR)\misc_vars.mod" "$(INTDIR)\globals.mod" "$(INTDIR)\gas_functions.mod"\
 "$(INTDIR)\date_time.mod" "$(INTDIR)\bed_module.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\scalars_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\profile_init.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_PROFI=\
	".\Release\globals.mod"\
	

"$(INTDIR)\profile_init.obj" : $(SOURCE) $(DEP_F90_PROFI) "$(INTDIR)"\
 "$(INTDIR)\globals.mod"


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_PROFI=\
	".\Debug\globals.mod"\
	

"$(INTDIR)\profile_init.obj" : $(SOURCE) $(DEP_F90_PROFI) "$(INTDIR)"\
 "$(INTDIR)\globals.mod"


!ENDIF 

SOURCE=.\scalars_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

F90_MODOUT=\
	"scalars"


"$(INTDIR)\scalars_module.obj"	"$(INTDIR)\scalars.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

F90_MODOUT=\
	"scalars"


"$(INTDIR)\scalars_module.obj"	"$(INTDIR)\scalars.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\scalars_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_SCALA=\
	".\bed_functions.inc"\
	".\Release\generic_source.mod"\
	".\Release\globals.mod"\
	".\Release\met_data_module.mod"\
	".\Release\misc_vars.mod"\
	".\Release\particulate_source.mod"\
	".\Release\scalars.mod"\
	".\Release\sediment_source.mod"\
	".\Release\tdg_source.mod"\
	".\Release\temperature_source.mod"\
	
F90_MODOUT=\
	"scalars_source"


"$(INTDIR)\scalars_source.obj"	"$(INTDIR)\scalars_source.mod" : $(SOURCE)\
 $(DEP_F90_SCALA) "$(INTDIR)" "$(INTDIR)\generic_source.mod"\
 "$(INTDIR)\particulate_source.mod" "$(INTDIR)\globals.mod"\
 "$(INTDIR)\misc_vars.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\sediment_source.mod"\
 "$(INTDIR)\tdg_source.mod" "$(INTDIR)\temperature_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_SCALA=\
	".\bed_functions.inc"\
	".\Debug\generic_source.mod"\
	".\Debug\globals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\particulate_source.mod"\
	".\Debug\scalars.mod"\
	".\Debug\sediment_source.mod"\
	".\Debug\tdg_source.mod"\
	".\Debug\temperature_source.mod"\
	
F90_MODOUT=\
	"scalars_source"


"$(INTDIR)\scalars_source.obj"	"$(INTDIR)\scalars_source.mod" : $(SOURCE)\
 $(DEP_F90_SCALA) "$(INTDIR)" "$(INTDIR)\generic_source.mod"\
 "$(INTDIR)\particulate_source.mod" "$(INTDIR)\globals.mod"\
 "$(INTDIR)\misc_vars.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\sediment_source.mod"\
 "$(INTDIR)\tdg_source.mod" "$(INTDIR)\temperature_source.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\sediment_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_SEDIM=\
	".\bed_functions.inc"\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	
F90_MODOUT=\
	"sediment_source"


"$(INTDIR)\sediment_source.obj"	"$(INTDIR)\sediment_source.mod" : $(SOURCE)\
 $(DEP_F90_SEDIM) "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\misc_vars.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_SEDIM=\
	".\bed_functions.inc"\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	
F90_MODOUT=\
	"sediment_source"


"$(INTDIR)\sediment_source.obj"	"$(INTDIR)\sediment_source.mod" : $(SOURCE)\
 $(DEP_F90_SEDIM) "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\misc_vars.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\table_bc_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_TABLE=\
	".\Release\date_time.mod"\
	
F90_MODOUT=\
	"table_boundary_conditions"


"$(INTDIR)\table_bc_module.obj"	"$(INTDIR)\table_boundary_conditions.mod" : \
$(SOURCE) $(DEP_F90_TABLE) "$(INTDIR)" "$(INTDIR)\date_time.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_TABLE=\
	".\Debug\date_time.mod"\
	
F90_MODOUT=\
	"table_boundary_conditions"


"$(INTDIR)\table_bc_module.obj"	"$(INTDIR)\table_boundary_conditions.mod" : \
$(SOURCE) $(DEP_F90_TABLE) "$(INTDIR)" "$(INTDIR)\date_time.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\tdg_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_TDG_S=\
	".\Release\gas_functions.mod"\
	".\Release\met_data_module.mod"\
	".\Release\misc_vars.mod"\
	
F90_MODOUT=\
	"tdg_source"


"$(INTDIR)\tdg_source.obj"	"$(INTDIR)\tdg_source.mod" : $(SOURCE)\
 $(DEP_F90_TDG_S) "$(INTDIR)" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\gas_functions.mod" "$(INTDIR)\met_data_module.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_TDG_S=\
	".\Debug\gas_functions.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\misc_vars.mod"\
	
F90_MODOUT=\
	"tdg_source"


"$(INTDIR)\tdg_source.obj"	"$(INTDIR)\tdg_source.mod" : $(SOURCE)\
 $(DEP_F90_TDG_S) "$(INTDIR)" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\gas_functions.mod" "$(INTDIR)\met_data_module.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\temperature_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_TEMPE=\
	".\Release\energy_flux.mod"\
	".\Release\met_data_module.mod"\
	".\Release\misc_vars.mod"\
	
F90_MODOUT=\
	"temperature_source"


"$(INTDIR)\temperature_source.obj"	"$(INTDIR)\temperature_source.mod" : \
$(SOURCE) $(DEP_F90_TEMPE) "$(INTDIR)" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\energy_flux.mod" "$(INTDIR)\met_data_module.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_TEMPE=\
	".\Debug\energy_flux.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\misc_vars.mod"\
	
F90_MODOUT=\
	"temperature_source"


"$(INTDIR)\temperature_source.obj"	"$(INTDIR)\temperature_source.mod" : \
$(SOURCE) $(DEP_F90_TEMPE) "$(INTDIR)" "$(INTDIR)\misc_vars.mod"\
 "$(INTDIR)\energy_flux.mod" "$(INTDIR)\met_data_module.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\total_conc.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_TOTAL=\
	".\Release\scalars.mod"\
	".\Release\scalars_source.mod"\
	

"$(INTDIR)\total_conc.obj" : $(SOURCE) $(DEP_F90_TOTAL) "$(INTDIR)"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\scalars_source.mod"


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_TOTAL=\
	".\Debug\scalars.mod"\
	".\Debug\scalars_source.mod"\
	

"$(INTDIR)\total_conc.obj" : $(SOURCE) $(DEP_F90_TOTAL) "$(INTDIR)"\
 "$(INTDIR)\scalars.mod" "$(INTDIR)\scalars_source.mod"


!ENDIF 

SOURCE=.\transport_only_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_TRANS=\
	".\Release\date_time.mod"\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	".\Release\scalars.mod"\
	
F90_MODOUT=\
	"transport_only"


"$(INTDIR)\transport_only_module.obj"	"$(INTDIR)\transport_only.mod" : \
$(SOURCE) $(DEP_F90_TRANS) "$(INTDIR)" "$(INTDIR)\date_time.mod"\
 "$(INTDIR)\globals.mod" "$(INTDIR)\misc_vars.mod" "$(INTDIR)\scalars.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_TRANS=\
	".\Debug\date_time.mod"\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\scalars.mod"\
	
F90_MODOUT=\
	"transport_only"


"$(INTDIR)\transport_only_module.obj"	"$(INTDIR)\transport_only.mod" : \
$(SOURCE) $(DEP_F90_TRANS) "$(INTDIR)" "$(INTDIR)\date_time.mod"\
 "$(INTDIR)\globals.mod" "$(INTDIR)\misc_vars.mod" "$(INTDIR)\scalars.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

