#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUI.h"
#include "GUIDEFS.h"

#include "mass2file.h"

extern Boolean_t STDCALL MASS2LoaderEngine(StringList_pa sl);

static void Dialog1HelpButton_CB(void)
{
  TecUtilLockOn();
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockOff();
}

static void Dialog1CancelButton_CB(void)
{
  /* Only unlock tecplot here because a modal dialog was launched. */
  GUI_DialogDismiss(Dialog1Manager);
  TecUtilLockOff();
}

static void Dialog1OkButton_CB(void)
{
  StringList_pa LoaderCMD = TecUtilStringListAlloc();
  char buffer[1024];
  int i;
  int nvar, var, *selvar;
  int *t, nt;
  Boolean_t IsOK;

                                /* include file name */

  sprintf(buffer, "F%s", mass2_file);
 TRACE1("Loader Commands: \"%s\"\n", buffer);
  (void)TecUtilStringListAppendString(LoaderCMD, buffer);

                                /* list selected vars */

  GUI_ListGetSelectedItems(VarList_MLST_D1, &selvar, &nvar);
  for (i = 0; i < nvar; i++) {
    int idx = selvar[i];
    if (idx > mass2_static_vars) {
      idx -= mass2_static_vars;
      idx -= 1;
      var = mass2_timedep_varid[idx];
    } else {
      idx -= 1;
      var = mass2_static_varid[idx];
    }
    sprintf(buffer, "V%s", mass2VarName(var));
    TRACE1("Loader Commands: \"%s\"\n", buffer);
    TecUtilStringListAppendString(LoaderCMD, buffer);
  }
  GUI_ListDeallocItemList(&selvar);

                                /* set start and end time slices */


  GUI_ListGetSelectedItems(StartList_SLST_D1, &t, &nt);
  i = *t - 1;
  /*
  i = GUI_OptionMenuGet(StartTime_OPT_D1) - 1;
  */
  sprintf(buffer, "S%d", i);
  TRACE1("Loader Commands: \"%s\"\n", buffer);
  TecUtilStringListAppendString(LoaderCMD, buffer);

  GUI_ListGetSelectedItems(EndList_SLST_D1, &t, &nt);
  i = *t - 1;
  /*
  i = GUI_OptionMenuGet(EndTime_OPT_D1) - 1;
  */
  sprintf(buffer, "E%d", i);
  TRACE1("Loader Commands: \"%s\"\n", buffer);
  TecUtilStringListAppendString(LoaderCMD, buffer);

  GUI_DialogDismiss(Dialog1Manager);

                                /* run the Loader */

  mass2CloseFile();

  IsOK = MASS2LoaderEngine(LoaderCMD);

  if (! IsOK) {
    (void)TecUtilDialogMessageBox("Error Loading MASS2 File", MessageBox_Error);
  } else {
    TecUtilDialogMessageBox("MASS2 File Successfully Imported", MessageBox_Information);
  }

  TecUtilStringListDealloc(&LoaderCMD);
  /* Only unlock tecplot here because a modal dialog was launched. */
  TecUtilLockOff();
}

static void VarList_MLST_D1_SetAll(void)
{
  int tmp[5000];
  int n = GUI_ListGetItemCount(VarList_MLST_D1);
  int i;

  for (i = 0; i < n; i++) tmp[i] = i + 1;
  GUI_ListSetSelectedItems(VarList_MLST_D1, tmp, n);
}

static void VarList_MLST_D1_SetNone(void)
{
  int tmp[1];
  GUI_ListDeselectAllItems(VarList_MLST_D1);
}

static void Dialog1Init_CB(void)
{
  int times, i, id;
  char buf[10240];

  /* Only lock here because this is a modal dialog being launched */
  TecUtilLockOn();

  GUI_ListDeleteAllItems(VarList_MLST_D1);

  for (i = 0; i < mass2_static_vars; i++) {
    GUI_ListAppendItem(VarList_MLST_D1, mass2VarName(mass2_static_varid[i]));
  }
  for (i = 0; i < mass2_timedep_vars; i++) {
    GUI_ListAppendItem(VarList_MLST_D1, mass2VarName(mass2_timedep_varid[i]));
  }

  GUI_ListDeleteAllItems(StartList_SLST_D1);
  GUI_ListDeleteAllItems(StartList_SLST_D1);

  times = mass2Times();
  TRACE1("Building timestamp lists: %d entries\n", mass2Times());
  for (i = 0; i < times; i++) {
    const char *s = mass2TimeStamp(i);
    TRACE1("Building timestamp lists: appending %s\n", s);
    GUI_ListAppendItem(StartList_SLST_D1, s);
    GUI_ListAppendItem(EndList_SLST_D1, s);
  }
  GUI_ListSetSelectedItem(StartList_SLST_D1, 1);
  GUI_ListSetSelectedItem(EndList_SLST_D1, mass2Times());
  /*
  GUI_OptionMenuSet(StartTime_OPT_D1, 1);
  GUI_OptionMenuSet(EndTime_OPT_D1, mass2Times());
  */
  VarList_MLST_D1_SetAll();
}

static void VarList_MLST_D1_CB(const int *I)
{
  TecUtilLockOn();
  TRACE1("Multi selection list (VarList_MLST_D1) item selected,  First Item is: %d\n",*I);
  TecUtilLockOff();
}

static void EndList_SLST_D1_CB(const int *I)
{
  int n, *s;
  TecUtilLockOn();
  TRACE1("Single selection list (EndList_SLST_D1) item selected,  Item is: %d\n",*I);
  GUI_ListGetSelectedItems(StartList_SLST_D1, &s, &n);
  if (*I < *s) {
    GUI_ListSetSelectedItem(StartList_SLST_D1, *I);
  }
  TecUtilLockOff();
}


static void StartList_SLST_D1_CB(const int *I)
{
  int n, *e;
  TecUtilLockOn();
  TRACE1("Single selection list (StartList_SLST_D1) item selected,  Item is: %d\n",*I);
  GUI_ListGetSelectedItems(EndList_SLST_D1, &e, &n);
  if (*I > *e) {
    GUI_ListSetSelectedItem(EndList_SLST_D1, *I);
  }
  TecUtilLockOff();
}

/*
char StartTime_OPT_D1_List[100000];
static void StartTime_OPT_D1_CB(const int *I)
{
  int e;

  TecUtilLockOn();
  TRACE1("Option Menu (StartTime_OPT_D1) value changed,  New value is: %d\n",*I);
  e = GUI_OptionMenuGet(EndTime_OPT_D1);
  if (*I > e) {
    GUI_OptionMenuSet(EndTime_OPT_D1, *I);
  }
  TecUtilLockOff();
}

char EndTime_OPT_D1_List[100000];
static void EndTime_OPT_D1_CB(const int *I)
{
  int s;
  TecUtilLockOn();
  TRACE1("Option Menu (EndTime_OPT_D1) value changed,  New value is: %d\n",*I);

  s = GUI_OptionMenuGet(StartTime_OPT_D1);
  if (s > *I) {
    GUI_OptionMenuSet(StartTime_OPT_D1, *I);
  }
  TecUtilLockOff();
}

*/

#include "guibld.c"
