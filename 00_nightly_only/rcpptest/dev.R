

# http://adv-r.had.co.nz/Rcpp.html


library(Rcpp)
library(microbenchmark)
#>
#> Attaching package: 'Rcpp'
#> The following object is masked from 'package:inline':
#>
#>     registerPlugin
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')
# add works like a regular R function
add
#> function (x, y, z)
#> .Call(<pointer: 0x7f96ecb3ef20>, x, y, z)
add(1, 2, 3)
#> [1] 6


sumR <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  total
}



cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')


x <- runif(1e3)
microbenchmark(
  sum(x),
  sumC(x),
  sumR(x)
)


pdistR <- function(x, ys) {
  sqrt((x - ys) ^ 2)
}

cppFunction('NumericVector pdistC(double x, NumericVector ys) {
    int n = ys.size();
    NumericVector out(n);

    for(int i = 0; i < n; ++i) {
      out[i] = sqrt(pow(ys[i] - x, 2.0));
    }
    return out;
  }')




cppFunction('CharacterVector f(String str) {
    int n = str.length();
    CharacterVector result( n );
    for ( int j = 0; j < n; j++ )
    {
        result[j] = str[n -j -1];
    }
    return result;
}')

cppFunction('
CharacterVector f( const std::string str) {
    int n = str.length();
    CharacterVector result( n );
    for ( int j = 0; j < n; j++ )
    {
        result[j] = str.substr( n-j-1, 1 );
    }
    return result;
}')



lcs_r_part <- function (s1, s2){
  if (nchar(s1) == 0 || nchar(s2) == 0)
    return("")
  v1 <- unlist(strsplit(s1, split = ""))
  v2 <- unlist(strsplit(s2, split = ""))
  num <- matrix(0, nchar(s1), nchar(s2))
  maxlen <- 0
  pstart = 0
  for (i in 1:nchar(s1)) {
    for (j in 1:nchar(s2)) {
      if (v1[i] == v2[j]) {
        if ((i == 1) || (j == 1)) {
          num[i, j] <- 1
        }
        else {
          num[i, j] <- 1 + num[i - 1, j - 1]
        }
        if (num[i, j] > maxlen) {
          maxlen <- num[i, j]
          pstart = i - maxlen + 1
        }
      }
    }
  }
  return(substr(s1, pstart, pstart + maxlen - 1))
}

lcs_r <- function(ss){
  Reduce(lcs_r_part, ss)
}

lcs_rc <- function(ss){
  Reduce(lcs_c, ss)
}


lcs_r(c("indr","i am indranil", "my name is indra"))

cppFunction('
String lcs_c( const std::string s1, const std::string s2) {
  int n1 = s1.length();
  int n2 = s2.length();
  String out = "";
  int maxlen = 0;
  int pstart = 0;
  int num[n1][n2] = {0};

  if(n1 > 0 && n2 > 0){
    out = "ok";
    for (int i=0; i<n1; i++)
    {
      for (int j=0; j<n2; j++)
      {

        if (i == 0 || j == 0)
          num[i][j] = 1;

        else if (s1[i] == s2[j])
        {
          num[i][j] = num[i-1][j-1] + 1;
        }

        if (num[i][j] - 1 > maxlen) {
          maxlen = num[i][j] - 1;
          pstart = i - maxlen+1;
        }

      }
    }

    out = s1.substr(pstart, maxlen);

  }
  return out;
}

')


cppFunction('
String lcs_c( const std::string s1, const std::string s2) {

  int n1 = s1.length();
  int n2 = s2.length();
  int dynM[n1 + 1][n2 + 1];
  std::string out = "";

  int len = 0;

  int row, col;

  for (int i = 0; i <= n1; i++) {
    for (int j = 0; j <= n2; j++) {
      if (i == 0 || j == 0)
        dynM[i][j] = 0;

      else if (s1[i - 1] == s2[j - 1]) {
        dynM[i][j] = dynM[i - 1][j - 1] + 1;
        if (len < dynM[i][j]) {
          len = dynM[i][j];
          row = i;
          col = j;
        }
      }
      else
        dynM[i][j] = 0;
    }
  }

  if (len == 0) {
    return out;
  }

// char* resultStr = (char*)std::malloc((len + 1) * sizeof(char));
 std::string resultStr (len,\'X\');

  while (dynM[row][col] != 0) {

    resultStr[--len] = s1[row - 1];

    row--;
    col--;
  }



  return resultStr;
}

')





microbenchmark(
  lcs_cc(c("indr","i am indranil", "my name is indra")),
  lcs_rc(c("indr","i am indranil", "my name is indra")),
  lcs_r(c("indr","i am indranil", "my name is indra")), times = 1000)


badvec <- c("indr","i am indranil", "my name is indra")[sample(3,10^3, replace = T)]


microbenchmark::microbenchmark(
  lcs_cc(badvec),
  lcs_rc(badvec),
  lcs_r(badvec))

bench::mark(
  lcs_cc(badvec),
  lcs_rc(badvec),
  lcs_r(badvec)
)


do_prep <- function(lst){
  lo <- list()
  lo$isnum <- lst %>% map_lgl(is.numeric)
  lo$list <- lst %>% map(as.character)
  lo
}

options(stringsAsFactors = F)
as_cell_df_c <- function(lst){
  lp <- do_prep(lst)
  tst(lp$list, lp$isnum)
}


as_cell_df_c2 <- function(df){
  dts <- df %>% map_chr(~ifelse(is.numeric(.x), "numeric","character"))
  fast_cdf(df, dts)
}

N <- 10^3+10^2*3+10*8

N<- 10^6
df <- data.frame(x=rnorm(N), y = rnorm(N))

# tidycells::as_cell_df(df, take_col = F)
# as_cell_df_c(df)

microbenchmark(tidycells::as_cell_df(df, take_col = F),
               as_cell_df_c(df), times = 1)




as_cell_df_r <- function(x) {
  val <- x %>% map(as.character) %>% do.call(c,.)
  #val <- x %>% map(as.character) %>% unlist()
  #val <- getlin(x)
  nr <- nrow(x)
  nc <- ncol(x)
  dtypes <- x %>% map_chr(~ifelse(is.numeric(.x), "numeric","character"))
  # Spread cells into different columns by data type
  out <- tibble::tibble(
    row = rep(seq_len(nr), times = nc),
    col = rep(seq_len(nc), each = nr),
    value = val,
    data_type = rep(dtypes, each = nr)
  )
  out

}

as_cell_df_r2 <- function(x) {
  #val <- x %>% map(as.character) %>% do.call(c,.)
  #val <- x %>% map(as.character) %>% unlist()
  val <- getlin(x)
  nr <- nrow(x)
  nc <- ncol(x)
  dtypes <- x %>% map_chr(~ifelse(is.numeric(.x), "numeric","character"))
  # Spread cells into different columns by data type
  out <- tibble::tibble(
    row = rep(seq_len(nr), times = nc),
    col = rep(seq_len(nc), each = nr),
    value = val,
    data_type = rep(dtypes, each = nr)
  )
  out

}



N<- 10^1
df <- data.frame(x=rnorm(N), y = rnorm(N))

# tidycells::as_cell_df(df, take_col = F)
# as_cell_df_c(df)

microbenchmark(unpivotr::as_cells(df) %>% tidycells::as_cell_df(),
               tidycells::as_cell_df(df, take_col = F) %>% tidycells::as_cell_df(),
               # as_cell_df_c(df) %>% tidycells::as_cell_df(),
               as_cell_df_c2(df) %>% tidycells::as_cell_df(),
               as_cell_df_r(df) %>% tidycells::as_cell_df(),
               as_cell_df_r2(df) %>% tidycells::as_cell_df(), times = 10)


