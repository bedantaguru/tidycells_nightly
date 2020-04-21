#include <Rcpp.h>

using namespace Rcpp;

// in case in sugar needs to be delete for warning
// as of now not in use
// it is slow
template < class T >
LogicalVector in_local(T x, T y) {
  int xl = x.length();
  LogicalVector out = no_init(xl);
  for (int i = 0; i < xl; i++) {
    if (std::find(y.begin(), y.end(), x[i]) != y.end())
      out[i] = true;
    else
      out[i] = false;
  }
  return out;
}

// cc is equivalent to c() in R
template < class T >
T cc(T xa, T xb) {
  int nra = xa.length();
  int nrb = xb.length();
  T x = no_init(nra + nrb);
  
  for (int i = 0; i < x.length(); i++) {
    if (i < nra) {
      x[i] = xa[i];
    } else {
      x[i] = xb[i - nra];
    }
  }
  return x;
}

// paste equivalent
// slow compared to paste
CharacterVector paste_local(NumericVector x, NumericVector y) {
  int i = 0, sz = x.size();
  CharacterVector res = no_init(sz);
  
  for (std::ostringstream oss; i < sz; i++, oss.str("")) {
    oss << x[i] << "_" << y[i];
    res[i] = oss.str();
  }
  
  return res;
}

// minimal implementation
DataFrame get_group_id_boundary_cpp(DataFrame drc_id) {
  
  NumericVector drc_id_row = drc_id["row"];
  NumericVector drc_id_col = drc_id["col"];
  NumericVector drc_id_row_grp_min = {
    min(drc_id_row)
  };
  NumericVector drc_id_row_grp_max = {
    max(drc_id_row)
  };
  NumericVector drc_id_col_grp_min = {
    min(drc_id_col)
  };
  NumericVector drc_id_col_grp_max = {
    max(drc_id_col)
  };
  
  DataFrame dfo = DataFrame::create(_("r_min") = drc_id_row_grp_min,
                                    _("r_max") = drc_id_row_grp_max,
                                    _("c_min") = drc_id_col_grp_min,
                                    _("c_max") = drc_id_col_grp_max);
  
  return dfo;
}

bool is_attachable_single_logic_2_sub(DataFrame d_com, DataFrame whole_data) {
  // note get_group_id_boundary_cpp may be slower on larger number of groups
  
  NumericVector d_com_row = d_com["row"];
  NumericVector d_com_col = d_com["col"];
  
  CharacterVector d_com_row_col = paste_local(d_com_row, d_com_col);
  
  DataFrame gbd = get_group_id_boundary_cpp(d_com);
  NumericVector gbd_r_min = gbd["r_min"];
  NumericVector gbd_r_max = gbd["r_max"];
  NumericVector gbd_c_min = gbd["c_min"];
  NumericVector gbd_c_max = gbd["c_max"];
  
  // @Dev whole_data can be ripped
  NumericVector whole_data_row = whole_data["row"];
  NumericVector whole_data_col = whole_data["col"];
  
  CharacterVector whole_data_row_col = paste_local(whole_data_row, whole_data_col);
  
  int nr_whole_data = whole_data_row.length();
  LogicalVector whole_data_sel_rows = no_init(nr_whole_data);
  LogicalVector whole_data_sel_rows2 = no_init(nr_whole_data);
  
  whole_data_sel_rows = (whole_data_row >= rep(gbd_r_min, nr_whole_data)) &
    (whole_data_row <= rep(gbd_r_max, nr_whole_data)) &
    (whole_data_col >= rep(gbd_c_min, nr_whole_data)) &
    (whole_data_col <= rep(gbd_c_max, nr_whole_data));
  
  whole_data_sel_rows2 = !( in (whole_data_row_col, d_com_row_col));
  
  if (is_true(any(whole_data_sel_rows & whole_data_sel_rows2))) {
    
    return false;
  }
  
  return true;
}

bool is_attachable_single_logic_3_sub(CharacterVector this_dir,
                                      NumericVector d_dat_row_filt, NumericVector d_dat_col_filt,
                                      DataFrame d_att,
                                      CharacterVector direction, CharacterVector attr_gid,
                                      DataFrame whole_data) {
  
  int dl = direction.length();
  LogicalVector sel_dm = (direction == rep(this_dir, dl));
  if (is_true(any(sel_dm))) {
    
    CharacterVector attr_gid_sel = attr_gid[sel_dm];
    CharacterVector d_att_gid = d_att["gid"];
    NumericVector d_att_row = d_att["row"];
    NumericVector d_att_col = d_att["col"];
    LogicalVector d_att_rows = in (d_att_gid, attr_gid_sel);
    
    NumericVector d_att_row_sel = d_att_row[d_att_rows];
    NumericVector d_att_col_sel = d_att_col[d_att_rows];
    
    NumericVector row_cc = cc(d_att_row_sel, d_dat_row_filt);
    NumericVector col_cc = cc(d_att_col_sel, d_dat_col_filt);
    
    DataFrame d_comb = DataFrame::create(_("row") = row_cc, _("col") = col_cc);
    
    bool chk_out = is_attachable_single_logic_2_sub(d_comb, whole_data);
    return chk_out;
    
  }
  
  return true;
  
}

bool is_attachable_single(std::string gid1, std::string gid2,
                          DataFrame d_dat, DataFrame d_att, DataFrame data_attr_map,
                          DataFrame whole_data) {
  
  //  ########## logic #########
  //  #### should have similar major sides (of attributes)
  
  CharacterVector attr_group = data_attr_map["attr_group"];
  CharacterVector data_gid = data_attr_map["data_gid"];
  CharacterVector direction = data_attr_map["direction"];
  
  CharacterVector major_str = {
    "major"
  }, gid1r = gid1, gid2r = gid2;
  
  int nr_data_attr_map = attr_group.length();
  
  LogicalVector data_attr_map_sel_rows_0 = (attr_group == rep(major_str, nr_data_attr_map));
  LogicalVector data_attr_map_sel_rows_1 = (data_gid == rep(gid1r, nr_data_attr_map)) & data_attr_map_sel_rows_0;
  LogicalVector data_attr_map_sel_rows_2 = (data_gid == rep(gid2r, nr_data_attr_map)) & data_attr_map_sel_rows_0;
  
  CharacterVector direction_1_1 = direction[data_attr_map_sel_rows_1];
  CharacterVector direction_2_1 = direction[data_attr_map_sel_rows_2];
  
  CharacterVector direction_1_2 = sort_unique(direction_1_1);
  CharacterVector direction_2_2 = sort_unique(direction_2_1);
  
  if (is_false(all(direction_1_2 == direction_2_2))) {
    
    return false;
  }
  
  //  ########## logic #########
  //  #### if any intersecting cell present
  //  #### it will be duplication if logic fails
  
  CharacterVector d_dat_gid = d_dat["gid"];
  NumericVector d_dat_row = d_dat["row"];
  NumericVector d_dat_col = d_dat["col"];
  int nr_d_dat = d_dat_row.length();
  LogicalVector d_dat_row_sel = (d_dat_gid == rep(gid1r, nr_d_dat)) | (d_dat_gid == rep(gid2r, nr_d_dat));
  NumericVector d_dat_row_filt = d_dat_row[d_dat_row_sel];
  NumericVector d_dat_col_filt = d_dat_col[d_dat_row_sel];
  
  DataFrame d_com = DataFrame::create(_("row") = d_dat_row_filt,
                                      _("col") = d_dat_col_filt);
  
  if (!is_attachable_single_logic_2_sub(d_com, whole_data)) {
    return false;
  }
  
  //  ########## logic #########
  //  #### should have no other entry within the enclosed combined boundary (direction-wise)
  //  #### should have no other entry (non empty) within the enclosed combined boundary attaching major attributes (direction-wise)
  
  LogicalVector data_attr_map_sel_rows_3 = ((data_gid == rep(gid1r, nr_data_attr_map)) | (data_gid == rep(gid2r, nr_data_attr_map)));
  CharacterVector attr_gid = data_attr_map["attr_gid"];
  
  CharacterVector direction_this = direction[data_attr_map_sel_rows_3];
  CharacterVector attr_gid_this = attr_gid[data_attr_map_sel_rows_3];
  
  CharacterVector dirs = {
    "N",
    "E",
    "W",
    "S"
  };
  LogicalVector chks = no_init(4);
  for (int i = 0; i < 4; i++) {
    CharacterVector td = {
      dirs[i]
    };
    chks[i] = is_attachable_single_logic_3_sub(td, d_dat_row_filt, d_dat_col_filt, d_att, direction_this, attr_gid_this, whole_data);
  }
  
  if (is_true(any(!chks))) {
    return false;
  }
  
  return true;
}

//' is_attachable for multiple rows of gid1 gid2 composition
//'
//' @param g1g2df a df having 'gid1' 'gid2' columns
//' @keywords internal
// [[Rcpp::export]]
LogicalVector is_attachable_multiple_cpp(DataFrame g1g2df,
                                         DataFrame d_dat, DataFrame d_att, DataFrame data_attr_map,
                                         DataFrame whole_data) {
  int nr = g1g2df.nrow();
  LogicalVector out = no_init(nr);
  std::vector < std::string > gid1 = g1g2df["gid1"], gid2 = g1g2df["gid2"];
  
  for (int i = 0; i < nr; i++) {
    out[i] = is_attachable_single(gid1[i], gid2[i], d_dat, d_att, data_attr_map, whole_data);
  }
  return out;
}