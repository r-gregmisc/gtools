#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern void C_checkStatus(
    int status,
    char* status_str,
    int status_len
);

extern void C_setTCPNoDelay(
    int *socket,
    int* flag,
    int* status,
    char** status_str,
    int* status_len
);
  
extern void C_convert(
    char**  letters,
    int*  nchar,
    int*  values
);

extern void C_roman2int(
    char** str,
    int*  nchar,
    int*  retval
);

static R_NativePrimitiveArgType C_convert_t[] = {
  STRSXP, // letters
  INTSXP, // nchar
  INTSXP  // values
};

static R_NativePrimitiveArgType C_roman2int_t[] = {
  STRSXP, // str
  INTSXP, // nchar
  INTSXP  // retval
};

static R_NativePrimitiveArgType C_checkStatus_t[] = {
    INTSXP, // status
    STRSXP, // status_str
    INTSXP  // status_len
};

static R_NativePrimitiveArgType C_setTCPNoDelay_t[] = {
  INTSXP, // socket
  INTSXP, // flag
  INTSXP, // status
  STRSXP, // status_str
  INTSXP  // status_len
};


static const R_CMethodDef cMethods[] = {
  {"C_convert",       (DL_FUNC) &C_convert,       3, C_convert_t      },
  {"C_roman2int",     (DL_FUNC) &C_roman2int,     3, C_roman2int_t    },
  {"C_checkStatus",   (DL_FUNC) &C_checkStatus,   3, C_checkStatus_t  },
  {"C_setTCPNoDelay", (DL_FUNC) &C_setTCPNoDelay, 5, C_setTCPNoDelay_t},
  {NULL, NULL, 0, NULL}
};


void R_init_gtools(DllInfo *dll)
{
  R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE); // to prevent dynamic lookup
}