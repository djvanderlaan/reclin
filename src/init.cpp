#include "r_export.h"
#include <R_ext/Rdynload.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

extern "C" {

  static const R_CallMethodDef r_calldef[] = {
     CALLDEF(greedy_logical_cpp, 2),
     {NULL, NULL, 0}
  };

  void R_init_lvec(DllInfo *info) {
    R_registerRoutines(info, NULL, r_calldef, NULL, NULL);
    R_useDynamicSymbols(info, static_cast<Rboolean>(FALSE));
  }
}

