# Microsoft Developer Studio Project File - Name="mass2_v025" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=mass2_v025 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "mass2.mak".
!MESSAGE 
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

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /include:"Release/" /compile_only /nologo /warn:nofileopt
# ADD F90 /include:"Release/" /include:"E:\Software\NetCDF\include" /compile_only /nologo /warn:nofileopt
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 netcdfs.lib kernel32.lib /nologo /subsystem:console /machine:I386 /nodefaultlib:"libcmt.lib" /out:"Release/mass2_v027.exe" /libpath:"E:\Software\NetCDF\lib"

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /include:"Debug/" /compile_only /nologo /debug:full /optimize:0 /warn:nofileopt
# ADD F90 /include:"Debug/" /include:"E:\Software\NetCDF\include" /compile_only /nologo /debug:full /optimize:0 /warn:nofileopt
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 netcdfs.lib kernel32.lib /nologo /subsystem:console /debug /machine:I386 /nodefaultlib:"libcmt.lib" /out:"Debug/mass2_v027.exe" /pdbtype:sept /libpath:"E:\Software\NetCDF\lib"

!ENDIF 

# Begin Target

# Name "mass2_v025 - Win32 Release"
# Name "mass2_v025 - Win32 Debug"
# Begin Source File

SOURCE=.\accumulator.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_ACCUM=\
	".\Release\bed_module.mod"\
	".\Release\date_time.mod"\
	".\Release\gas_functions.mod"\
	".\Release\globals.mod"\
	".\Release\met_data_module.mod"\
	".\Release\misc_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\scalars_source.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_ACCUM=\
	".\Debug\bed_module.mod"\
	".\Debug\date_time.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\globals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\scalars_source.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\bed.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_BED_F=\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\scalars_source.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_BED_F=\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\scalars_source.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\bed_functions.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_BED_FU=\
	".\Release\bed_module.mod"\
	".\Release\scalars_source.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_BED_FU=\
	".\Debug\bed_module.mod"\
	".\Debug\scalars_source.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\bed_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_BED_S=\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	".\Release\table_boundary_conditions.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_BED_S=\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\table_boundary_conditions.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\block_bc_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_BLOCK=\
	".\Release\table_boundary_conditions.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_BLOCK=\
	".\Debug\table_boundary_conditions.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\date_time_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_DATE_=\
	".\Release\julian.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_DATE_=\
	".\Debug\julian.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\energy_flux_module.f90
# End Source File
# Begin Source File

SOURCE=.\gage_output_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_GAGE_=\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
NODEP_F90_GAGE_=\
	".\Release\bed_module.mod"\
	".\Release\date_time.mod"\
	".\Release\gas_functions.mod"\
	".\Release\globals.mod"\
	".\Release\scalars.mod"\
	".\Release\scalars_source.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_GAGE_=\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
NODEP_F90_GAGE_=\
	".\Debug\bed_module.mod"\
	".\Debug\date_time.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\globals.mod"\
	".\Debug\scalars.mod"\
	".\Debug\scalars_source.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\gas_coeffs_module.f90
# End Source File
# Begin Source File

SOURCE=.\gas_functions_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_GAS_F=\
	".\Release\gas_coeffs.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_GAS_F=\
	".\Debug\gas_coeffs.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\generic_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_GENER=\
	".\Release\bed_source.mod"\
	".\Release\misc_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_GENER=\
	".\Debug\bed_source.mod"\
	".\Debug\misc_vars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\global_module_023.f90
# End Source File
# Begin Source File

SOURCE=.\io_routines_module.f90
# End Source File
# Begin Source File

SOURCE=.\julian.f90
# End Source File
# Begin Source File

SOURCE=.\mass2.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_MASS2=\
	".\Release\mass2_main_025.mod"\
	".\Release\misc_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_MASS2=\
	".\Debug\mass2_main_025.mod"\
	".\Debug\misc_vars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\mass2_main_025.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_MASS2_=\
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
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_MASS2_=\
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
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\met_data_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_MET_D=\
	".\Release\date_time.mod"\
	".\Release\table_boundary_conditions.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_MET_D=\
	".\Debug\date_time.mod"\
	".\Debug\table_boundary_conditions.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\misc_vars_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_MISC_=\
	".\Release\date_time.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_MISC_=\
	".\Debug\date_time.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\netcdferror.f90
DEP_F90_NETCD=\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\particulate_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_PARTI=\
	".\bed_functions.inc"\
	
NODEP_F90_PARTI=\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\sediment_source.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_PARTI=\
	".\bed_functions.inc"\
	
NODEP_F90_PARTI=\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\sediment_source.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\plot_output.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_PLOT_=\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
NODEP_F90_PLOT_=\
	".\Release\accumulator.mod"\
	".\Release\bed_module.mod"\
	".\Release\date_time.mod"\
	".\Release\gas_functions.mod"\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	".\Release\scalars.mod"\
	".\Release\scalars_source.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_PLOT_=\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
NODEP_F90_PLOT_=\
	".\Debug\accumulator.mod"\
	".\Debug\bed_module.mod"\
	".\Debug\date_time.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\scalars.mod"\
	".\Debug\scalars_source.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\profile_init.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_PROFI=\
	".\Release\globals.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_PROFI=\
	".\Debug\globals.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\scalars_module.f90
# End Source File
# Begin Source File

SOURCE=.\scalars_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_SCALA=\
	".\Release\Biota_Mod.mod"\
	".\Release\biota_source_module.mod"\
	".\Release\generic_source.mod"\
	".\Release\globals.mod"\
	".\Release\met_data_module.mod"\
	".\Release\misc_vars.mod"\
	".\Release\particulate_source.mod"\
	".\Release\scalars.mod"\
	".\Release\sediment_source.mod"\
	".\Release\tdg_source.mod"\
	".\Release\temperature_source.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_SCALA=\
	".\Debug\Biota_Mod.mod"\
	".\Debug\biota_source_module.mod"\
	".\Debug\generic_source.mod"\
	".\Debug\globals.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\particulate_source.mod"\
	".\Debug\scalars.mod"\
	".\Debug\sediment_source.mod"\
	".\Debug\tdg_source.mod"\
	".\Debug\temperature_source.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\sediment_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_SEDIM=\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	".\Release\scalars.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_SEDIM=\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\scalars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\table_bc_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_TABLE=\
	".\Release\date_time.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_TABLE=\
	".\Debug\date_time.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\tdg_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_TDG_S=\
	".\Release\gas_functions.mod"\
	".\Release\met_data_module.mod"\
	".\Release\misc_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_TDG_S=\
	".\Debug\gas_functions.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\misc_vars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\temperature_source.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_TEMPE=\
	".\Release\energy_flux.mod"\
	".\Release\met_data_module.mod"\
	".\Release\misc_vars.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_TEMPE=\
	".\Debug\energy_flux.mod"\
	".\Debug\met_data_module.mod"\
	".\Debug\misc_vars.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\total_conc.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_TOTAL=\
	".\Release\scalars.mod"\
	".\Release\scalars_source.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_TOTAL=\
	".\Debug\scalars.mod"\
	".\Debug\scalars_source.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\transport_only_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_TRANS=\
	".\Release\date_time.mod"\
	".\Release\globals.mod"\
	".\Release\misc_vars.mod"\
	".\Release\scalars.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

NODEP_F90_TRANS=\
	".\Debug\date_time.mod"\
	".\Debug\globals.mod"\
	".\Debug\misc_vars.mod"\
	".\Debug\scalars.mod"\
	

!ENDIF 

# End Source File
# End Target
# End Project
