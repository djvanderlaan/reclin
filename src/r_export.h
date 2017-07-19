#ifndef r_export_h
#define r_export_h

#include <R.h>
#include <Rinternals.h>

extern "C" {
  SEXP greedy_logical_cpp(SEXP ra, SEXP rb);
}

#endif


