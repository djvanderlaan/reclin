
#include <R.h>
#include <Rinternals.h>
#include <string>
#include <exception>
#include <vector>
#include <algorithm>
#include <cstring>
#include <stdexcept>
#include "r_export.h"

// A class implenting RAII to protect and unprotect R-objects
// Use: protSEXP p = function_that_returns_an_sexp()
// and PROTECT and UNPROTECT are needed any mode.
class protSEXP {
  public:
    protSEXP(SEXP p) : p_(PROTECT(p)) { }
    ~protSEXP() { UNPROTECT_PTR(p_); }
    operator SEXP() const { return p_; }
    SEXP get() { return p_;}
  private:
    SEXP p_;
};

// Pair of macros that can be used to catch any remaining exceptions and pass
// these on to R. Start and end functions that get called by R by these.
#define RTRY \
    try {
#define RCATCH \
    } catch(const std::string& e) { \
      error(e.c_str()); \
      return R_NilValue; \
    } catch (...) { \
      error("Uncaught exception."); \
      return R_NilValue; \
    } \


class PairList {
  public:
    PairList() {
      size_ = 10000;
      a_ = new int[size_];
      b_ = new int[size_];
      last_ = 0;
    };

    ~PairList() { 
      delete [] a_;
      delete [] b_;
    };

    bool duplicate(int a, int b) const {
      // first check a
      int* p = a_;
      for (unsigned int i = 0; i < last_; ++i, ++p) {
        if ((*p) == a) return true;
      }
      // then check b
      p = b_;
      for (unsigned int i = 0; i < last_; ++i, ++p) {
        if ((*p) == b) return true;
      }
      return false;
    };

    void add(int a, int b) {
      if (last_ >= size_) {
        // resize
        unsigned int new_size = size_ * 2;
        int* new_a = new int[new_size];
        int* new_b = new int[new_size];
        std::memcpy(new_a, a_, size_ * sizeof(int));
        std::memcpy(new_b, b_, size_ * sizeof(int));
        std::swap(a_, new_a);
        std::swap(b_, new_b);
        size_ = new_size;
        delete [] new_a;
        delete [] new_b;
      }
      a_[last_] = a;
      b_[last_] = b;
      ++last_;
      return;
    };
    
    int* a() { return a_;}
    int* b() { return b_;}
    unsigned int size() { return last_;}
    
  private:
    unsigned int size_;
    int* a_;
    int* b_;
    unsigned int last_;
};

// extern "C" {
//   SEXP greedy_cpp(SEXP ra, SEXP rb) {
//     RTRY
//     int* a = INTEGER(ra);
//     int* b = INTEGER(rb);
//     int n = LENGTH(ra);
//     if (LENGTH(rb) != n) 
//       throw std::runtime_error("Lengths of a and b do not match");
//     
//     PairList list;
//     int* pa = a;
//     int* pb = b;
//     for (int i = 0; i < n; ++i, ++pa, ++pb) {
//       if (!list.duplicate(*pa, *pb)) list.add(*pa, *pb);
//     }
//     
//     SEXP res_ra = PROTECT(allocVector(INTSXP, list.size()));
//     SEXP res_rb = PROTECT(allocVector(INTSXP, list.size()));
//     int* res_a = INTEGER(res_ra);
//     int* res_b = INTEGER(res_rb);
//     std::memcpy(res_a, list.a(), list.size() * sizeof(int));
//     std::memcpy(res_b, list.b(), list.size() * sizeof(int));
//     
//     SEXP res = PROTECT(allocVector(VECSXP, 2));
//     SET_VECTOR_ELT(res, 0, res_ra);
//     SET_VECTOR_ELT(res, 1, res_rb);
//     UNPROTECT(3);
//     return res;
//     RCATCH
//   }
// };


extern "C" {
  SEXP greedy_logical_cpp(SEXP ra, SEXP rb) {
    RTRY
    int* a = INTEGER(ra);
    int* b = INTEGER(rb);
    int n = LENGTH(ra);
    if (LENGTH(rb) != n) 
      throw std::runtime_error("Lengths of a and b do not match");
    
    protSEXP res = allocVector(LGLSXP, n);
    int* pr = LOGICAL(res);
    
    PairList list;
    int* pa = a;
    int* pb = b;
    for (int i = 0; i < n; ++i, ++pa, ++pb, ++pr) {
      if (!list.duplicate(*pa, *pb)) {
        list.add(*pa, *pb); 
        (*pr) = 1;
      } else {
        (*pr) = 0;
      }
    }
    return res;
    RCATCH
  }
}


