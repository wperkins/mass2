# Microsoft Developer Studio Generated NMAKE File, Based on solver_test.dsp
!IF "$(CFG)" == ""
CFG=solver_test - Win32 Debug
!MESSAGE No configuration specified. Defaulting to solver_test - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "solver_test - Win32 Release" && "$(CFG)" !=\
 "solver_test - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "solver_test.mak" CFG="solver_test - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "solver_test - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "solver_test - Win32 Debug" (based on\
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

!IF  "$(CFG)" == "solver_test - Win32 Release"

OUTDIR=.\solver_t
INTDIR=.\solver_t
# Begin Custom Macros
OutDir=.\solver_t
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\solver_test.exe"

!ELSE 

ALL : "$(OUTDIR)\solver_test.exe"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\solver_backup.obj"
	-@erase "$(INTDIR)\solver_common.mod"
	-@erase "$(INTDIR)\solver_common.obj"
	-@erase "$(INTDIR)\solver_module.mod"
	-@erase "$(INTDIR)\solver_tdma.obj"
	-@erase "$(INTDIR)\solver_test.obj"
	-@erase "$(OUTDIR)\solver_test.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /optimize:3\
 /warn:nofileopt /module:"solver_t/" /object:"solver_t/" 
F90_OBJS=.\solver_t/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\solver_test.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\solver_test.pdb" /machine:I386 /out:"$(OUTDIR)\solver_test.exe"\
 
LINK32_OBJS= \
	"$(INTDIR)\solver_backup.obj" \
	"$(INTDIR)\solver_common.obj" \
	"$(INTDIR)\solver_tdma.obj" \
	"$(INTDIR)\solver_test.obj"

"$(OUTDIR)\solver_test.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "solver_test - Win32 Debug"

OUTDIR=.\solver_0
INTDIR=.\solver_0
# Begin Custom Macros
OutDir=.\solver_0
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\solver_test.exe"

!ELSE 

ALL : "$(OUTDIR)\solver_test.exe"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\solver_backup.obj"
	-@erase "$(INTDIR)\solver_common.mod"
	-@erase "$(INTDIR)\solver_common.obj"
	-@erase "$(INTDIR)\solver_module.mod"
	-@erase "$(INTDIR)\solver_tdma.obj"
	-@erase "$(INTDIR)\solver_test.obj"
	-@erase "$(OUTDIR)\solver_test.exe"
	-@erase "$(OUTDIR)\solver_test.ilk"
	-@erase "$(OUTDIR)\solver_test.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /debug:full /optimize:0\
 /warn:nofileopt /module:"solver_0/" /object:"solver_0/"\
 /pdbfile:"solver_0/DF50.PDB" 
F90_OBJS=.\solver_0/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\solver_test.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\solver_test.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)\solver_test.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\solver_backup.obj" \
	"$(INTDIR)\solver_common.obj" \
	"$(INTDIR)\solver_tdma.obj" \
	"$(INTDIR)\solver_test.obj"

"$(OUTDIR)\solver_test.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.SUFFIXES: .fpp

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  


!IF "$(CFG)" == "solver_test - Win32 Release" || "$(CFG)" ==\
 "solver_test - Win32 Debug"
SOURCE=.\solver_backup.f90

"$(INTDIR)\solver_backup.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\solver_common.f90

!IF  "$(CFG)" == "solver_test - Win32 Release"

F90_MODOUT=\
	"solver_common"


"$(INTDIR)\solver_common.obj"	"$(INTDIR)\solver_common.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "solver_test - Win32 Debug"

F90_MODOUT=\
	"solver_common"


"$(INTDIR)\solver_common.obj"	"$(INTDIR)\solver_common.mod" : $(SOURCE)\
 "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\solver_tdma.f90

!IF  "$(CFG)" == "solver_test - Win32 Release"

DEP_F90_SOLVE=\
	".\solver_t\solver_common.mod"\
	
F90_MODOUT=\
	"solver_module"


"$(INTDIR)\solver_tdma.obj"	"$(INTDIR)\solver_module.mod" : $(SOURCE)\
 $(DEP_F90_SOLVE) "$(INTDIR)" "$(INTDIR)\solver_common.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "solver_test - Win32 Debug"

DEP_F90_SOLVE=\
	".\solver_0\solver_common.mod"\
	
F90_MODOUT=\
	"solver_module"


"$(INTDIR)\solver_tdma.obj"	"$(INTDIR)\solver_module.mod" : $(SOURCE)\
 $(DEP_F90_SOLVE) "$(INTDIR)" "$(INTDIR)\solver_common.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\solver_test.f90

!IF  "$(CFG)" == "solver_test - Win32 Release"

DEP_F90_SOLVER=\
	".\solver_t\solver_module.mod"\
	

"$(INTDIR)\solver_test.obj" : $(SOURCE) $(DEP_F90_SOLVER) "$(INTDIR)"\
 "$(INTDIR)\solver_module.mod"


!ELSEIF  "$(CFG)" == "solver_test - Win32 Debug"

DEP_F90_SOLVER=\
	".\solver_0\solver_module.mod"\
	

"$(INTDIR)\solver_test.obj" : $(SOURCE) $(DEP_F90_SOLVER) "$(INTDIR)"\
 "$(INTDIR)\solver_module.mod"


!ENDIF 


!ENDIF 

