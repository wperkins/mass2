# Microsoft Developer Studio Generated NMAKE File, Based on mass2_v025.dsp
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
!MESSAGE NMAKE /f "mass2_v025.mak" CFG="mass2_v025 - Win32 Debug"
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

ALL : "$(OUTDIR)\mass2_v025.exe"

!ELSE 

ALL : "$(OUTDIR)\mass2_v025.exe"

!ENDIF 

CLEAN :
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
	-@erase "$(INTDIR)\global_module_023.obj"
	-@erase "$(INTDIR)\globals.mod"
	-@erase "$(INTDIR)\io_routines_module.mod"
	-@erase "$(INTDIR)\io_routines_module.obj"
	-@erase "$(INTDIR)\julian.mod"
	-@erase "$(INTDIR)\julian.obj"
	-@erase "$(INTDIR)\mass2_main_025.obj"
	-@erase "$(INTDIR)\met_data_module.mod"
	-@erase "$(INTDIR)\met_data_module.obj"
	-@erase "$(INTDIR)\plot_output.mod"
	-@erase "$(INTDIR)\plot_output.obj"
	-@erase "$(INTDIR)\profile_init.obj"
	-@erase "$(INTDIR)\scalars.mod"
	-@erase "$(INTDIR)\scalars_module.obj"
	-@erase "$(INTDIR)\table_bc_module.obj"
	-@erase "$(INTDIR)\table_boundary_conditions.mod"
	-@erase "$(OUTDIR)\mass2_v025.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /warn:nofileopt\
 /module:"Release/" /object:"Release/" 
F90_OBJS=.\Release/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mass2_v025.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\mass2_v025.pdb" /machine:I386 /out:"$(OUTDIR)\mass2_v025.exe" 
LINK32_OBJS= \
	"$(INTDIR)\block_bc_module.obj" \
	"$(INTDIR)\date_time_module.obj" \
	"$(INTDIR)\energy_flux_module.obj" \
	"$(INTDIR)\gage_output_module.obj" \
	"$(INTDIR)\gas_coeffs_module.obj" \
	"$(INTDIR)\gas_functions_module.obj" \
	"$(INTDIR)\global_module_023.obj" \
	"$(INTDIR)\io_routines_module.obj" \
	"$(INTDIR)\julian.obj" \
	"$(INTDIR)\mass2_main_025.obj" \
	"$(INTDIR)\met_data_module.obj" \
	"$(INTDIR)\plot_output.obj" \
	"$(INTDIR)\profile_init.obj" \
	"$(INTDIR)\scalars_module.obj" \
	"$(INTDIR)\table_bc_module.obj"

"$(OUTDIR)\mass2_v025.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

ALL : "$(OUTDIR)\mass2_v025.exe" "$(OUTDIR)\DF50.PDB"

!ELSE 

ALL : "$(OUTDIR)\mass2_v025.exe" "$(OUTDIR)\DF50.PDB"

!ENDIF 

CLEAN :
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
	-@erase "$(INTDIR)\global_module_023.obj"
	-@erase "$(INTDIR)\globals.mod"
	-@erase "$(INTDIR)\io_routines_module.mod"
	-@erase "$(INTDIR)\io_routines_module.obj"
	-@erase "$(INTDIR)\julian.mod"
	-@erase "$(INTDIR)\julian.obj"
	-@erase "$(INTDIR)\mass2_main_025.obj"
	-@erase "$(INTDIR)\met_data_module.mod"
	-@erase "$(INTDIR)\met_data_module.obj"
	-@erase "$(INTDIR)\plot_output.mod"
	-@erase "$(INTDIR)\plot_output.obj"
	-@erase "$(INTDIR)\profile_init.obj"
	-@erase "$(INTDIR)\scalars.mod"
	-@erase "$(INTDIR)\scalars_module.obj"
	-@erase "$(INTDIR)\table_bc_module.obj"
	-@erase "$(INTDIR)\table_boundary_conditions.mod"
	-@erase "$(OUTDIR)\mass2_v025.exe"
	-@erase "$(OUTDIR)\mass2_v025.ilk"
	-@erase "$(OUTDIR)\mass2_v025.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /debug:full /optimize:0\
 /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF50.PDB" 
F90_OBJS=.\Debug/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mass2_v025.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\mass2_v025.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)\mass2_v025.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\block_bc_module.obj" \
	"$(INTDIR)\date_time_module.obj" \
	"$(INTDIR)\energy_flux_module.obj" \
	"$(INTDIR)\gage_output_module.obj" \
	"$(INTDIR)\gas_coeffs_module.obj" \
	"$(INTDIR)\gas_functions_module.obj" \
	"$(INTDIR)\global_module_023.obj" \
	"$(INTDIR)\io_routines_module.obj" \
	"$(INTDIR)\julian.obj" \
	"$(INTDIR)\mass2_main_025.obj" \
	"$(INTDIR)\met_data_module.obj" \
	"$(INTDIR)\plot_output.obj" \
	"$(INTDIR)\profile_init.obj" \
	"$(INTDIR)\scalars_module.obj" \
	"$(INTDIR)\table_bc_module.obj"

"$(OUTDIR)\mass2_v025.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

F90_MODOUT=\
	"gage_output"


"$(INTDIR)\gage_output_module.obj"	"$(INTDIR)\gage_output.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

F90_MODOUT=\
	"gage_output"


"$(INTDIR)\gage_output_module.obj"	"$(INTDIR)\gage_output.mod" : $(SOURCE)\
 "$(INTDIR)"
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

SOURCE=.\mass2_main_025.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_MASS2=\
	".\Release\block_boundary_conditions.mod"\
	".\Release\date_time.mod"\
	".\Release\energy_flux.mod"\
	".\Release\gage_output.mod"\
	".\Release\gas_functions.mod"\
	".\Release\globals.mod"\
	".\Release\io_routines_module.mod"\
	".\Release\met_data_module.mod"\
	".\Release\plot_output.mod"\
	".\Release\scalars.mod"\
	".\Release\table_boundary_conditions.mod"\
	

"$(INTDIR)\mass2_main_025.obj" : $(SOURCE) $(DEP_F90_MASS2) "$(INTDIR)"\
 "$(INTDIR)\globals.mod" "$(INTDIR)\io_routines_module.mod"\
 "$(INTDIR)\block_boundary_conditions.mod"\
 "$(INTDIR)\table_boundary_conditions.mod" "$(INTDIR)\date_time.mod"\
 "$(INTDIR)\gage_output.mod" "$(INTDIR)\plot_output.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\energy_flux.mod"\
 "$(INTDIR)\gas_functions.mod"


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_MASS2=\
	".\Debug\block_boundary_conditions.mod"\
	".\Debug\date_time.mod"\
	".\Debug\energy_flux.mod"\
	".\Debug\gage_output.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\globals.mod"\
	".\Debug\io_routines_module.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\plot_output.mod"\
	".\Debug\scalars.mod"\
	".\Debug\table_boundary_conditions.mod"\
	

"$(INTDIR)\mass2_main_025.obj" : $(SOURCE) $(DEP_F90_MASS2) "$(INTDIR)"\
 "$(INTDIR)\globals.mod" "$(INTDIR)\io_routines_module.mod"\
 "$(INTDIR)\block_boundary_conditions.mod"\
 "$(INTDIR)\table_boundary_conditions.mod" "$(INTDIR)\date_time.mod"\
 "$(INTDIR)\gage_output.mod" "$(INTDIR)\plot_output.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\met_data_module.mod" "$(INTDIR)\energy_flux.mod"\
 "$(INTDIR)\gas_functions.mod"


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
 $(DEP_F90_MET_D) "$(INTDIR)" "$(INTDIR)\table_boundary_conditions.mod"\
 "$(INTDIR)\date_time.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\plot_output.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_PLOT_=\
	".\Release\gas_functions.mod"\
	".\Release\globals.mod"\
	".\Release\scalars.mod"\
	
F90_MODOUT=\
	"plot_output"


"$(INTDIR)\plot_output.obj"	"$(INTDIR)\plot_output.mod" : $(SOURCE)\
 $(DEP_F90_PLOT_) "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\gas_functions.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_PLOT_=\
	".\Debug\gas_functions.mod"\
	".\Debug\globals.mod"\
	".\Debug\scalars.mod"\
	
F90_MODOUT=\
	"plot_output"


"$(INTDIR)\plot_output.obj"	"$(INTDIR)\plot_output.mod" : $(SOURCE)\
 $(DEP_F90_PLOT_) "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\scalars.mod"\
 "$(INTDIR)\gas_functions.mod"
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


!ENDIF 

