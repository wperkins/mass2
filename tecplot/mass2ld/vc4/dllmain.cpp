// -------------------------------------------------------------
// file: dllmain.cpp
// -------------------------------------------------------------
// -------------------------------------------------------------
// Battelle Memorial Institute
// Pacific Northwest Laboratory
// -------------------------------------------------------------
// -------------------------------------------------------------
// Created August 27, 1999 by William A. Perkins
// Last Change: Fri Aug 27 15:39:31 1999 by William A. Perkins <d3g096@R101243>
// -------------------------------------------------------------


static const char* RCS_ID = "$Id$ Battelle PNL";

#include <windows.h>

BOOL APIENTRY 
DllMain( HANDLE hModule, DWORD ul_reason_for_call, LPVOID lpReserved )
{
    switch( ul_reason_for_call ) {
    case DLL_PROCESS_ATTACH:
      break;
    case DLL_THREAD_ATTACH:
      break;
    case DLL_THREAD_DETACH:
      break;
    case DLL_PROCESS_DETACH:
      break;
    }
    return TRUE;
}
