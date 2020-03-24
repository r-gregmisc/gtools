#include "gtools.h"

void R_init_gtools(DllInfo *info)
{
  /* Register C routines */
  R_registerRoutines (info, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols     (info, TRUE);
}

void R_unload_gtools(DllInfo *info)
{
  /* Release resources. */
}
