# Microsoft Developer Studio Generated NMAKE File, Based on cart_grid.dsp
!IF "$(CFG)" == ""
CFG=cart_grid - Win32 Debug
!MESSAGE No configuration specified. Defaulting to cart_grid - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "cart_grid - Win32 Release" && "$(CFG)" !=\
 "cart_grid - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "cart_grid.mak" CFG="cart_grid - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "cart_grid - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "cart_grid - Win32 Debug" (based on "Win32 (x86) Console Application")
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

!IF  "$(CFG)" == "cart_grid - Win32 Release"

OUTDIR=.\..\..\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\..\..\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\cart_grid.exe"

!ELSE 

ALL : "$(OUTDIR)\cart_grid.exe"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\cartgrid.obj"
	-@erase "$(OUTDIR)\cart_grid.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /warn:nofileopt\
 /module:"Release/" /object:"Release/" 
F90_OBJS=.\Release/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\cart_grid.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\cart_grid.pdb" /machine:I386 /out:"$(OUTDIR)\cart_grid.exe" 
LINK32_OBJS= \
	"$(INTDIR)\cartgrid.obj"

"$(OUTDIR)\cart_grid.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "cart_grid - Win32 Debug"

OUTDIR=.\..\..\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\..\..\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\cart_grid.exe" "$(OUTDIR)\DF50.PDB"

!ELSE 

ALL : "$(OUTDIR)\cart_grid.exe" "$(OUTDIR)\DF50.PDB"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\cartgrid.obj"
	-@erase "$(OUTDIR)\cart_grid.exe"
	-@erase "$(OUTDIR)\cart_grid.ilk"
	-@erase "$(OUTDIR)\cart_grid.pdb"
	-@erase "..\..\Debug\DF50.PDB"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /debug:full /optimize:0\
 /warn:nofileopt /module:"Debug/" /object:"Debug/"\
 /pdbfile:"..\..\Debug/DF50.PDB" 
F90_OBJS=.\Debug/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\cart_grid.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\cart_grid.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)\cart_grid.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\cartgrid.obj"

"$(OUTDIR)\cart_grid.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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


!IF "$(CFG)" == "cart_grid - Win32 Release" || "$(CFG)" ==\
 "cart_grid - Win32 Debug"
SOURCE=.\cartgrid.f90

"$(INTDIR)\cartgrid.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

