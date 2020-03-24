#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void setTCPNoDelay(int *socket,
                     int* flag,
                     int* status,
                     char** status_str,
                     int* status_len
);

void convert(char**  letters,
             int*  nchar,
             int*  values
);


void roman2int(char** str,
               int*  nchar,
               int*  retval);


R_CMethodDef cMethods[] = {
  {"setTCPNoDelay", (DL_FUNC) &setTCPNoDelay, 5},
  {"convert",       (DL_FUNC) &convert,       3},
  {"roman2int",     (DL_FUNC) &roman2int,     3},
  {NULL, NULL, 0}
};

