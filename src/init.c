#include "gtools.h"


void R_init_gtools(DllInfo *dll)
{
  R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE); // to prevent dynamic lookup
}


void R_unload_gtools(DllInfo *info)
{
  /* Release resources. */
}
