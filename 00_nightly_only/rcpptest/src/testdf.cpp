#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame tst(List df, LogicalVector isNumCols) {

  //Environment base("package:base");
  //Function isNum = base["is.numeric"];
  //const int ncol = df.length();
  //CharacterVector classin = df.attr("class");
  //CharacterVector dfc = "data.frame";

  IntegerVector row(0);
  IntegerVector col(0);
  CharacterVector data_type(0);
  CharacterVector value(0);

  for( int j = 0;j<df.length(); j++){
    CharacterVector this_col = df[j];
    LogicalVector is_na_this_col = is_na(this_col);
    for( int i = 0; i < this_col.length(); i++){

      if(!is_na_this_col[i]){
        row.push_back(i+1);
        col.push_back(j+1);
        if(isNumCols[j]){
          data_type.push_back("numeric");
        }else{
          data_type.push_back("character");
        }
        value.push_back(this_col[i]);

      }
    }
  }

  DataFrame dfo = DataFrame::create(_("row")=row,
                                    _("col")=col,
                                    _("data_type")=data_type,
                                    _("value")=value);

  return dfo;
}



// [[Rcpp::export]]
CharacterVector getlin(List df) {
  int nc = df.length();
  CharacterVector acol = df[0];
  int nr = acol.length();
  CharacterVector out(nc*nr);

  for(int i = 0; i < nc ; i++){
    IntegerVector idx = seq(i*nr, i*nr+nr-1);
    CharacterVector thiscol = df[i];
    out[idx] = thiscol;
  }


  return out;
}

// [[Rcpp::export]]
DataFrame fast_cdf(DataFrame df, CharacterVector dtypes){
  CharacterVector val = getlin(df);
  int nc = df.length();
  int nr = val.length()/nc;

  IntegerVector row = rep(seq_len(nr), nc);
  IntegerVector col = rep_each(seq_len(nc), nr);
  CharacterVector data_type = rep_each(dtypes, nr);

  DataFrame dfo = DataFrame::create(_("row")=row,
                                    _("col")=col,
                                    _("data_type")=data_type,
                                    _("value")=val);

  return dfo;

}
// // We can now use the BH package
// // [[Rcpp::depends(BH)]]
//
// #include <Rcpp.h>
// #include <boost/lexical_cast.hpp>  	// one file, automatically found for me
//
// using namespace Rcpp;
//
// using boost::lexical_cast;
// using boost::bad_lexical_cast;
//
// // [[Rcpp::export]]
// std::vector<double> lexicalCast(std::vector<std::string> v) {
//
//   std::vector<double> res(v.size());
//
//   for (unsigned int i=0; i<v.size(); i++) {
//     try {
//       res[i] = lexical_cast<double>(v[i]);
//     } catch(bad_lexical_cast &) {
//       res[i] = NA_REAL;
//     }
//   }
//
//   return res;
// }

//
// #include <Rcpp.h>
// using namespace Rcpp;
//
// // [[Rcpp::export]]
// CharacterMatrix df2String(DataFrame x) {
//
//   int nCols = x.size();
//   CharacterVector chContents = as<CharacterVector>(x[1]);
//
//   int nRows = chContents.size();
//   CharacterMatrix chOut(nRows, nCols);
//
//   for (int i = 0; i < nCols; i++) {
//     chContents = as<CharacterVector>(x[i]);
//     chOut(_, i) = chContents;
//   }
//
//   return chOut;
// }
