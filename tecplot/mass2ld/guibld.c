/***************************************************
 *                                                 *
 *  NOTE!  This file is automatically built by     *
 *         the Tecplot GUI Builder.  It is highly  *
 *         recommended that you never edit this    *
 *         file directly!                          *
 *                                                 *
 ***************************************************/


void BuildDialog1(int  ParentDialog)
{
  if (Dialog1Manager != BADDIALOGID)
    return;
  Dialog1Manager = GUI_DialogCreateModal(ParentDialog,
                                          5662,
                                          1584,
                                          "MASS2 Loader",
                                          Dialog1HelpButton_CB,
                                          Dialog1OkButton_CB,
                                          Dialog1CancelButton_CB,
                                          Dialog1Init_CB);
  VarList_MLST_D1 = GUI_ListAdd(Dialog1Manager,
                                179,
                                220,
                                5069,
                                786,
                       1,
                       VarList_MLST_D1_CB);
  VariablestoI_LBL_D1 = GUI_LabelAdd(Dialog1Manager,
                                     284,
                                     94,
                    "Variables to Include:");
  StartTime_OPT_D1 = GUI_OptionMenuAdd(Dialog1Manager,
                                       2886,
                                       1069,
                                       2616,
                                       169,
                        StartTime_OPT_D1_List,
                        StartTime_OPT_D1_CB);
  StartTimePla_LBL_D1 = GUI_LabelAdd(Dialog1Manager,
                                     284,
                                     1126,
                    "Start Time Plane: ");
  EndTimePlane_LBL_D1 = GUI_LabelAdd(Dialog1Manager,
                                     284,
                                     1365,
                    "End Time Plane: ");
  EndTime_OPT_D1 = GUI_OptionMenuAdd(Dialog1Manager,
                                     2886,
                                     1308,
                                     2616,
                                     169,
                        EndTime_OPT_D1_List,
                        EndTime_OPT_D1_CB);
}

void InitTGB(void)
{
/* Currently not used */
}
