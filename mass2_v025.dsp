# Microsoft Developer Studio Project File - Name="mass2_v025" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=mass2_v025 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "mass2_v025.mak".
!MESSAGE 
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
# ADD LINK32 netcdfs.lib kernel32.lib /nologo /subsystem:console /machine:I386 /nodefaultlib:"libcmt.lib" /libpath:"E:\Software\NetCDF\lib"

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
# ADD LINK32 netcdfs.lib kernel32.lib /nologo /subsystem:console /debug /machine:I386 /nodefaultlib:"libcmt.lib" /pdbtype:sept /libpath:"E:\Software\NetCDF\lib"

!ENDIF 

# Begin Target

# Name "mass2_v025 - Win32 Release"
# Name "mass2_v025 - Win32 Debug"
# Begin Source File

SOURCE=.\block_bc_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_BLOCK=\
	".\Release\table_boundary_conditions.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_BLOCK=\
	".\Debug\table_boundary_conditions.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\date_time_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_DATE_=\
	".\Release\julian.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_DATE_=\
	".\Debug\julian.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\energy_flux_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\gage_output_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_GAGE_=\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
NODEP_F90_GAGE_=\
	".\Release\date_time.mod"\
	".\Release\gas_functions.mod"\
	".\Release\globals.mod"\
	".\Release\scalars.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_GAGE_=\
	".\Debug\date_time.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\globals.mod"\
	".\Debug\scalars.mod"\
	"E:\Software\NetCDF\include\netcdf.inc"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\gas_coeffs_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\gas_functions_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_GAS_F=\
	".\Release\gas_coeffs.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_GAS_F=\
	".\Debug\gas_coeffs.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\global_module_023.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\io_routines_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\julian.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\mass2_main_025.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_MASS2=\
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
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\met_data_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_MET_D=\
	".\Release\date_time.mod"\
	".\Release\table_boundary_conditions.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_MET_D=\
	".\Debug\date_time.mod"\
	".\Debug\table_boundary_conditions.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\netcdferror.f90
DEP_F90_NETCD=\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\plot_output.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

DEP_F90_PLOT_=\
	"E:\Software\NetCDF\include\netcdf.inc"\
	
NODEP_F90_PLOT_=\
	".\Release\date_time.mod"\
	".\Release\gas_functions.mod"\
	".\Release\globals.mod"\
	".\Release\scalars.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_PLOT_=\
	".\Debug\date_time.mod"\
	".\Debug\gas_functions.mod"\
	".\Debug\globals.mod"\
	".\Debug\scalars.mod"\
	"E:\Software\NetCDF\include\netcdf.inc"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\profile_init.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_PROFI=\
	".\Release\globals.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_PROFI=\
	".\Debug\globals.mod"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\scalars_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\table_bc_module.f90

!IF  "$(CFG)" == "mass2_v025 - Win32 Release"

NODEP_F90_TABLE=\
	".\Release\date_time.mod"\
	

!ELSEIF  "$(CFG)" == "mass2_v025 - Win32 Debug"

DEP_F90_TABLE=\
	".\Debug\date_time.mod"\
	

!ENDIF 

# End Source File
# End Target
# End Project
