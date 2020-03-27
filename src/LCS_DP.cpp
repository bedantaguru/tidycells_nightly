#include <Rcpp.h>
using namespace Rcpp;

std::string lcs_c_prt(const std::string s1, const std::string s2)
{
  
  const int n1 = s1.length();
  const int n2 = s2.length();
  
  IntegerMatrix dynM(n1 + 1, n2 + 1);
  
  int len = 0;
  
  int row, col;
  
  for (int i = 0; i <= n1; i++) {
    for (int j = 0; j <= n2; j++) {
      if (i == 0 || j == 0)
        dynM(i, j) = 0;
      
      else if (s1[i - 1] == s2[j - 1]) {
        dynM(i, j) = dynM(i - 1, j - 1) + 1;
        if (len < dynM(i, j)) {
          len = dynM(i, j);
          row = i;
          col = j;
        }
      }
      else
        dynM(i, j) = 0;
    }
  }
  
  if (len == 0) {
    return "";
  }
  
  std::string out(len, 'X');
  
  while (dynM(row, col) != 0) {
    out[--len] = s1[row - 1];
    
    row--;
    col--;
  }
  
  return out;
}

//' Longest Common Substring using dynamic programming
//'
//' @param ss a string vector.
//' @keywords internal
//' @examples 
//' LCS(c("hi","hello and hi","only hi"))
// [[Rcpp::export]]
String LCS(const std::vector<std::string> ss)
{
  int n = ss.size();
  int i = 0;
  String out = "";
  
  if (n == 1) {
    out = ss[0];
  }
  else if (n > 1) {
    out = ss[0];
    do {
      i += 1;
      if (i > n - 1) {
        break;
      }
      
      out = lcs_c_prt(out, ss[i]);
      
    } while (out != "");
  }
  
  return out;
}
