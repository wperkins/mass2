/* -------------------------------------------------------------
   file: mass2ld.c
   ------------------------------------------------------------- */
/* -------------------------------------------------------------
   Battelle Memorial Institute
   Pacific Northwest Laboratory
   ------------------------------------------------------------- */
/* -------------------------------------------------------------
   Created August 17, 1999 by William A. Perkins
   Last Change: Tue Aug 24 14:50:21 1999 by William A. Perkins <perk@mack.pnl.gov>
   ------------------------------------------------------------- */

static const char* RCS_ID = "$Id$ Battelle PNL";

#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUI.h"
#include "GUIDEFS.h"
#define ADDON_NAME "MASS2 Loader"
#define ADDON_VERSION "0.1"
#ifdef __DATE__
# define ADDON_DATE __DATE__
#else
#  ifdef DATE
#    define ADDON_DATE DATE
#  else
#    define ADDON_DATE ""
#  endif  /* DATE  */
#endif /* __DATE__ */

#include "mass2file.h"

                                /* from the GUI */

extern char StartTime_OPT_D1_List[];
extern char EndTime_OPT_D1_List[];

/* -------------------------------------------------------------
   MASS2LoaderEngine
   ------------------------------------------------------------- */
Boolean_t STDCALL 
MASS2LoaderEngine(StringList_pa cmdlist)
{
  Boolean_t IsOK = 1;
  int i, starttime, endtime;
  char *cmd;
  char filename[1024];
  int varids[100];
  StringList_pa varlist;

  TecUtilLockOn();

  varlist = TecUtilStringListAlloc();

                                /* always include x and y as vars */

  TecUtilStringListAppendString(varlist, "x");
  TecUtilStringListAppendString(varlist, "y");

                                /* handle the option strings */

  for (i = 1; i <= TecUtilStringListGetCount(cmdlist); i++) {
    cmd = TecUtilStringListGetString(cmdlist, i);
    TRACE1("Loader Processing Command: %s\n", cmd);
    switch (*cmd) {
    case 'F':
      strncpy(filename, cmd+1, 1024);
      break;
    case 'V':
      TecUtilStringListAppendString(varlist, cmd+1);
      break;
    case 'S':
      starttime = atoi(cmd+1);
      break;
    case 'E':
      endtime = atoi(cmd+1);
      break;
    default:
      IsOK = 0;
    }
  }

  if (IsOK) {
    if (mass2OpenFile(filename) != 0) {
      char msg[1024];
      sprintf(msg, "Unable to open MASS2 File: %s", mass2_error);
      TecUtilDialogMessageBox(msg, MessageBox_Error);
      IsOK = 0;
    } else {
      TRACE1("Loader: \"%s\" successfully opened\n", filename);
    }
  }

  if (IsOK && !TecUtilDataSetCreate("MASS2", varlist, TRUE)) {
    TecUtilDialogMessageBox("Could not create MASS2 Dataset", MessageBox_Error);
    IsOK = 0;
  } else {
    TRACE1("Loader: data set %s created\n", "MASS2");
  }

  if (IsOK) {
    int t, blk, i, j, vars, v, zone = 1, blocks;
    FieldDataType_e *fd_types;
    double *dv;
    float *fv;
    double part, total;

    vars = TecUtilStringListGetCount(varlist);
    blocks = mass2Blocks();
    fd_types = (FieldDataType_e *)calloc(vars, sizeof(FieldDataType_e));
    fd_types[0] = FieldDataType_Double;
    fd_types[1] = FieldDataType_Double;
    for (i = 2; i < vars; i++) fd_types[i] = FieldDataType_Float;
    
    dv = (double *) calloc(mass2MaxEta()*mass2MaxXi(), sizeof(double));
    fv = (float *) calloc(mass2MaxEta()*mass2MaxXi(), sizeof(float));

    TecUtilDialogLaunchPercentDone("Loading MASS2 Data ...", TRUE);

    total = vars*blocks*(endtime - starttime + 1);
    part = 0;
    for (t = starttime; t <= endtime && IsOK; t++) {
      for (blk = 0; blk < blocks && IsOK; blk++, zone++) {
        char zonename[1024];
        int maxeta, maxxi;

        maxeta = mass2BlkEta(blk);
        maxxi = mass2BlkXi(blk);

        sprintf(zonename, "%s Block %d", mass2TimeStamp(t), blk + 1);

        IsOK = TecUtilDataSetAddZone(zonename, 
                                     maxeta, maxxi, 1,
                                     ZoneType_Ordered, fd_types);

        if (IsOK) {
          TRACE1("Loader: Created Zone \"%s\"\n", zonename);
        } else {
          TecUtilDialogMessageBox("Error creating zone", MessageBox_Error);
        }
        
        TRACE1("Loader: Doing Block %d ", blk);
        TRACE1("which has max eta = %d ", maxeta);
        TRACE1("and max xi = %d\n", maxxi);

        for (v = 0; v < vars && IsOK; v++) {

          part += 1.0;
          
          if (!TecUtilDialogCheckPercentDone(part/total*100.0)) {
            IsOK = FALSE;
            break;
          }

          TRACE1("Loader: loading var \"%s\"\n", 
                 TecUtilStringListGetString(varlist, v+1));
            
          switch (v) {
          case 0:               /* x is double*/
            IsOK = mass2GetX(blk, dv);
            break;
          case 1:               /* y is double*/
            IsOK = mass2GetY(blk, dv);
            break;
          default:              /* all else is float */
            IsOK = mass2GetVar(t, blk, 
                               TecUtilStringListGetString(varlist, v+1), 
                               fv);
            break;
          }

          if (!IsOK) {
            TRACE1("Loader: failure to read variable \"%s\"\n",
                   TecUtilStringListGetString(varlist, v+1));
          }

          for (i = 0; i < maxeta && IsOK; i++) {
            for (j = 0; j < maxxi && IsOK; j++) {
              double value;

              /*
              TRACE1("Loader: writing var \"%s\" ", 
                     TecUtilStringListGetString(varlist, v+1));
              TRACE1("(%3d, ", i);
              TRACE1("%3d)\n", j);
              */
              switch (v) {
              case 0:
              case 1:
                value = dv[i*maxxi + j];
                break;
              default:
                value = fv[i*maxxi + j];
                break;
              }
              TecUtilDataValueSetByZoneVar((EntIndex_t)zone, 
                                           (EntIndex_t)v + 1,
                                           (LgIndex_t) (i*maxxi + j + 1),
                                           value);
            }
          }
        }
      }
    }
    TecUtilDialogDropPercentDone();
    free(fd_types);
    free(dv);
    free(fv);
  }
      
  if (IsOK) {
    Set_pa zone_set;

    mass2CloseFile();

    TecUtilImportSetLoaderInstr(ADDON_NAME, cmdlist);
    TecUtilFrameSetMode(Frame_TwoD);
    
    zone_set = TecUtilSetAlloc(TRUE);
    for (i = 1; i <= mass2Blocks(); i++) {
      TecUtilSetAddMember(zone_set, i, TRUE);
    }
    TecUtilZoneSetActive(zone_set, AssignOp_Equals);
    
    TecUtilSetDealloc(&zone_set);
    TecUtilRedraw(TRUE);
  }


  TecUtilStringListDealloc(&varlist);
  TecUtilLockOff();
  return IsOK;
}

/* -------------------------------------------------------------
   LaunchMASS2LoaderInterface
   ------------------------------------------------------------- */
void STDCALL 
LaunchMASS2LoaderInterface(void)
{
  int filedone = 0;
  char *fname;

  int ncstat, ncid;
  char msg[1024];
  
  TecUtilLockOn();
  
                                /* open file dialog to get name */

  while (! filedone) {
    fname = NULL;
    if (TecUtilDialogGetFileName(SelectFileOption_ReadSingleFile,
                                 &fname, 
                                 "MASS2 Output",
                                 NULL,
                                 "*.nc")){
      filedone = (! mass2OpenFile(fname));
      if (mass2OpenFile(fname)) {
        sprintf(msg, "Unable to open \"%s\":\n%s", fname, mass2_error);
        (void)TecUtilDialogMessageBox(msg, MessageBox_Error);
      } else {
        int i;
        int ntimes = mass2Times();
        char buffer[100000];
        TRACE1("Building timestamp menu lists: %d entries\n", mass2Times());
        strcpy(buffer, "");
        for (i = 0; i < ntimes; i++) {
          const char *s = mass2TimeStamp(i);
          TRACE1("Building timestamp menu lists: appending %s\n", s);
          if (i > 0) strcat(buffer, ",");
          strcat(buffer, s);
        }
        TRACE1("Building timestamp menu lists: duplicating \"%s\"\n", buffer);
        strcpy(StartTime_OPT_D1_List, buffer);
        strcpy(EndTime_OPT_D1_List, buffer);
        BuildDialog1(MAINDIALOGID);
        GUI_DialogLaunch(Dialog1Manager);
        filedone = 1;
      }
      TecUtilStringDealloc(&fname);
    } else {
      filedone = 1;
    }
  }

  TecUtilLockOff();
}
 
EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{
  MANAGESTATE /* Required for Windows MFC calls - Ignored in MOTIF */
  TecUtilLockOn();
  TecUtilAddOnRegisterInfo(ADDON_NAME,
                           "V"ADDON_VERSION" ("TecVersionId") "ADDON_DATE,
                           "Battelle Pacific NW Laboratories");

  TecUtilImportAddLoader(MASS2LoaderEngine, ADDON_NAME, 
                         LaunchMASS2LoaderInterface, NULL);
  InitTGB();

  TecUtilLockOff();
}
