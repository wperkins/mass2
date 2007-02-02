# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=mass2ld - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to mass2ld - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "mass2ld - Win32 Release" && "$(CFG)" !=\
 "mass2ld - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "mass2ld.mak" CFG="mass2ld - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mass2ld - Win32 Release" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "mass2ld - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "mass2ld - Win32 Debug"
CPP=cl.exe
RSC=rc.exe
MTL=mktyplib.exe

!IF  "$(CFG)" == "mass2ld - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\mass2ld.dll"

CLEAN : 
	-@erase ".\Release\mass2ld.dll"
	-@erase ".\Release\guidefs.obj"
	-@erase ".\Release\dllmain.obj"
	-@erase ".\Release\mass2ld.obj"
	-@erase ".\Release\guicb.obj"
	-@erase ".\Release\mass2file.obj"
	-@erase ".\Release\mass2ld.ilk"
	-@erase ".\Release\mass2ld.lib"
	-@erase ".\Release\mass2ld.exp"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "D:\Software\netcdf-3.5\include" /I "D:\Software\TEC90\Include" /I ".." /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "D:\Software\netcdf-3.5\include" /I\
 "D:\Software\TEC90\Include" /I ".." /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/mass2ld.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/mass2ld.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 D:\Software\netcdf-3.5\lib\netcdfs.lib D:\Software\TEC90\Bin\libtec.lib D:\Software\TEC90\Bin\Wingui.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /version:0.1 /subsystem:windows /dll /incremental:yes /machine:I386
LINK32_FLAGS=D:\Software\netcdf-3.5\lib\netcdfs.lib\
 D:\Software\TEC90\Bin\libtec.lib D:\Software\TEC90\Bin\Wingui.lib kernel32.lib\
 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib\
 ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /version:0.1\
 /subsystem:windows /dll /incremental:yes /pdb:"$(OUTDIR)/mass2ld.pdb"\
 /machine:I386 /out:"$(OUTDIR)/mass2ld.dll" /implib:"$(OUTDIR)/mass2ld.lib" 
LINK32_OBJS= \
	"$(INTDIR)/guidefs.obj" \
	"$(INTDIR)/dllmain.obj" \
	"$(INTDIR)/mass2ld.obj" \
	"$(INTDIR)/guicb.obj" \
	"$(INTDIR)/mass2file.obj"

"$(OUTDIR)\mass2ld.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mass2ld - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\mass2ld.dll"

CLEAN : 
	-@erase ".\Debug\vc40.pdb"
	-@erase ".\Debug\vc40.idb"
	-@erase ".\Debug\mass2ld.dll"
	-@erase ".\Debug\dllmain.obj"
	-@erase ".\Debug\mass2ld.obj"
	-@erase ".\Debug\guicb.obj"
	-@erase ".\Debug\mass2file.obj"
	-@erase ".\Debug\guidefs.obj"
	-@erase ".\Debug\mass2ld.ilk"
	-@erase ".\Debug\mass2ld.lib"
	-@erase ".\Debug\mass2ld.exp"
	-@erase ".\Debug\mass2ld.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "D:\Software\netcdf-3.5\include" /I "D:\Software\TEC90\Include" /I ".." /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "D:\Software\netcdf-3.5\include"\
 /I "D:\Software\TEC90\Include" /I ".." /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/mass2ld.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/mass2ld.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 D:\Software\netcdf-3.5\lib\netcdfs.lib D:\Software\TEC90\Bin\libtec.lib D:\Software\TEC90\Bin\Wingui.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /version:0.1 /subsystem:windows /dll /debug /machine:I386 /nodefaultlib:"libcmt.lib"
LINK32_FLAGS=D:\Software\netcdf-3.5\lib\netcdfs.lib\
 D:\Software\TEC90\Bin\libtec.lib D:\Software\TEC90\Bin\Wingui.lib kernel32.lib\
 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib\
 ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /version:0.1\
 /subsystem:windows /dll /incremental:yes /pdb:"$(OUTDIR)/mass2ld.pdb" /debug\
 /machine:I386 /nodefaultlib:"libcmt.lib" /out:"$(OUTDIR)/mass2ld.dll"\
 /implib:"$(OUTDIR)/mass2ld.lib" 
LINK32_OBJS= \
	"$(INTDIR)/dllmain.obj" \
	"$(INTDIR)/mass2ld.obj" \
	"$(INTDIR)/guicb.obj" \
	"$(INTDIR)/mass2file.obj" \
	"$(INTDIR)/guidefs.obj"

"$(OUTDIR)\mass2ld.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "mass2ld - Win32 Release"
# Name "mass2ld - Win32 Debug"

!IF  "$(CFG)" == "mass2ld - Win32 Release"

!ELSEIF  "$(CFG)" == "mass2ld - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\dllmain.cpp

"$(INTDIR)\dllmain.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE="\tmp\mass2-parallel\tecplot\mass2ld\guidefs.c"
DEP_CPP_GUIDE=\
	"\Software\TEC90\Include\TECADDON.h"\
	"\Software\TEC90\Include\GUI.h"\
	".\..\GUIDEFS.h"\
	"\Software\TEC90\Include\MASTER.h"\
	"\Software\TEC90\Include\GLOBAL.h"\
	"\Software\TEC90\Include\TECGLBL.h"\
	"\Software\TEC90\Include\ADDON.h"\
	"\Software\TEC90\Include\TECUTILO.h"\
	"\Software\TEC90\Include\TASSERT.h"\
	"\Software\TEC90\Include\TECUTILS.h"\
	"\Software\TEC90\Include\TECUTILQ.h"\
	"\Software\TEC90\Include\TECUTILM.h"\
	"\Software\TEC90\Include\VERSION.h"\
	"\Software\TEC90\Include\SV.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\guidefs.obj" : $(SOURCE) $(DEP_CPP_GUIDE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\tmp\mass2-parallel\tecplot\mass2ld\mass2file.c"
DEP_CPP_MASS2=\
	"\Software\netcdf-3.5\include\netcdf.h"\
	".\..\mass2file.h"\
	

"$(INTDIR)\mass2file.obj" : $(SOURCE) $(DEP_CPP_MASS2) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\tmp\mass2-parallel\tecplot\mass2ld\guicb.c"
DEP_CPP_GUICB=\
	"\Software\TEC90\Include\TECADDON.h"\
	".\..\ADDGLBL.h"\
	"\Software\TEC90\Include\GUI.h"\
	".\..\GUIDEFS.h"\
	".\..\mass2file.h"\
	".\..\guibld.c"\
	"\Software\TEC90\Include\MASTER.h"\
	"\Software\TEC90\Include\GLOBAL.h"\
	"\Software\TEC90\Include\TECGLBL.h"\
	"\Software\TEC90\Include\ADDON.h"\
	"\Software\TEC90\Include\TECUTILO.h"\
	"\Software\TEC90\Include\TASSERT.h"\
	"\Software\TEC90\Include\TECUTILS.h"\
	"\Software\TEC90\Include\TECUTILQ.h"\
	"\Software\TEC90\Include\TECUTILM.h"\
	"\Software\TEC90\Include\VERSION.h"\
	"\Software\TEC90\Include\SV.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\guicb.obj" : $(SOURCE) $(DEP_CPP_GUICB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\tmp\mass2-parallel\tecplot\mass2ld\mass2ld.c"
DEP_CPP_MASS2L=\
	"\Software\TEC90\Include\TECADDON.h"\
	".\..\ADDGLBL.h"\
	"\Software\TEC90\Include\GUI.h"\
	".\..\GUIDEFS.h"\
	".\..\mass2file.h"\
	"\Software\TEC90\Include\MASTER.h"\
	"\Software\TEC90\Include\GLOBAL.h"\
	"\Software\TEC90\Include\TECGLBL.h"\
	"\Software\TEC90\Include\ADDON.h"\
	"\Software\TEC90\Include\TECUTILO.h"\
	"\Software\TEC90\Include\TASSERT.h"\
	"\Software\TEC90\Include\TECUTILS.h"\
	"\Software\TEC90\Include\TECUTILQ.h"\
	"\Software\TEC90\Include\TECUTILM.h"\
	"\Software\TEC90\Include\VERSION.h"\
	"\Software\TEC90\Include\SV.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\mass2ld.obj" : $(SOURCE) $(DEP_CPP_MASS2L) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
