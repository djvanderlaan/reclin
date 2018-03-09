#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector greedy_rcpp(IntegerVector x, IntegerVector y) {
  std::unordered_set<int> xseen;
  std::unordered_set<int> yseen;
  LogicalVector res(x.length());
  int n = x.length();
  if (y.length() != n)
    throw std::runtime_error("Lengths of x and y do not match.");
  for (int i = 0; i < n; ++i) {
    if ((xseen.find(x[i]) == xseen.end()) && \
        (yseen.find(y[i]) == yseen.end())) {
      xseen.insert(x[i]);
      yseen.insert(y[i]);
      res[i] = true;
    } else res[i] = false;
  }
  return res;
}
