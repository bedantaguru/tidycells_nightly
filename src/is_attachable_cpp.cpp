#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame get_group_id_boundary_cpp(DataFrame drc_id){
  
  CharacterVector drc_id_gid = drc_id["gid"];
  int nr = drc_id_gid.length();
  CharacterVector drc_id_gid_uniq = unique(drc_id_gid);
  int nr_out = drc_id_gid_uniq.length();
  NumericVector drc_id_row= drc_id["row"];
  NumericVector drc_id_col= drc_id["col"];
  NumericVector drc_id_row_grp_min = no_init(nr_out);
  NumericVector drc_id_row_grp_max = no_init(nr_out);
  NumericVector drc_id_col_grp_min = no_init(nr_out);
  NumericVector drc_id_col_grp_max = no_init(nr_out);
  
  for(int i = 0; i < nr_out; i++){
    CharacterVector gid_sel = {drc_id_gid_uniq[i]};
    LogicalVector sel = (drc_id_gid == rep(gid_sel, nr));
    NumericVector drc_id_row_sel = drc_id_row[sel];
    NumericVector drc_id_col_sel = drc_id_col[sel];
    
    drc_id_row_grp_min[i] = min(drc_id_row_sel);
    drc_id_row_grp_max[i] = max(drc_id_row_sel);
    
    drc_id_col_grp_min[i] = min(drc_id_col_sel);
    drc_id_col_grp_max[i] = max(drc_id_col_sel);
  }
  
  //    summarise(r_min = min(row), c_min = min(col), r_max = max(row), c_max = max(col))
  DataFrame dfo = DataFrame::create(_("gid") = drc_id_gid_uniq,
                                    _("r_min") = drc_id_row_grp_min,
                                    _("r_max") = drc_id_row_grp_max,
                                    _("c_min") = drc_id_col_grp_min,
                                    _("c_max") = drc_id_col_grp_max);
  
  // CharacterVector tblcls = {"tbl_df",  "tbl", "data.frame"};
  // dfo.attr("class") = tblcls;
  return dfo;
}


template <class T>
T rbind_local_part(String cn,DataFrame a, DataFrame b){
  T xa = a[cn];
  T xb = b[cn];
  int nra = xa.length();
  int nrb = xb.length();
  T x = no_init(nra+nrb);
  
  for(int i = 0; i< x.length(); i++){
    if(i<nra){
      x[i] = xa[i];
    }else{
      x[i] = xb[i-nra];
    }
  }
  return x;
}


DataFrame rbind_local(DataFrame a, DataFrame b){
  
  CharacterVector gid = rbind_local_part<CharacterVector>("gid", a, b);
  NumericVector row = rbind_local_part<NumericVector>("row", a, b);
  NumericVector col = rbind_local_part<NumericVector>("col", a, b);
  
  
  DataFrame df = DataFrame::create(_("gid")=gid, _("row")=row, _("col")=col);
  return df;
}

bool is_attachable_single_logic_2_sub(DataFrame d_com, DataFrame whole_data){
  // note get_group_id_boundary_cpp may be slower on larger number of groups
  
  NumericVector d_com_row_filt = d_com["row"];
  NumericVector d_com_col_filt = d_com["col"];
  DataFrame gbd = get_group_id_boundary_cpp(d_com);
  NumericVector gbd_r_min = gbd["r_min"];
  NumericVector gbd_r_max = gbd["r_max"];
  NumericVector gbd_c_min = gbd["c_min"];
  NumericVector gbd_c_max = gbd["c_max"];
  
  NumericVector whole_data_row = whole_data["row"];
  NumericVector whole_data_col = whole_data["col"];
  int nr_whole_data = whole_data_row.length();
  LogicalVector whole_data_sel_rows = no_init(nr_whole_data);
  LogicalVector whole_data_sel_rows2 = no_init(nr_whole_data);
  
  whole_data_sel_rows = (whole_data_row >= rep(gbd_r_min, nr_whole_data)) &
    (whole_data_row <= rep(gbd_r_max, nr_whole_data)) &
    (whole_data_col >= rep(gbd_c_min, nr_whole_data)) &
    (whole_data_col <= rep(gbd_c_max, nr_whole_data));
  
  // waring or some issue coming from Rcpp::in
  // may need to write personal in
  whole_data_sel_rows2 = !(in(whole_data_row, d_com_row_filt) &
    in(whole_data_col, d_com_col_filt));
  
  if(is_true(any(whole_data_sel_rows & whole_data_sel_rows2))){
    
    return false;
  }
  
  return true;
}

bool is_attachable_single_logic_3_sub(CharacterVector this_dir, DataFrame data_attr_map_this){
  
  
  //  chks <- c("N","E","W","S") %>% map_lgl(~{
  CharacterVector  direction =   data_attr_map_this["direction"];
  CharacterVector  attr_gid =   data_attr_map_this["attr_gid"];
  LogicalVector sel_dm = direction==rep(this_dir,direction.length());
  if(is_true(any(sel_dm))){
    
    CharacterVector  attr_gid_sel =  attr_gid[sel_dm];
    
    
  }
  
  return true;
  
  //
  //    dm0 <- data_attr_map_this %>% filter(direction == .x)
  //
  //    if(nrow(dm0)>0){
  //      this_group_info <- d_dat %>% filter(gid %in% c(gid1, gid2)) %>%
  //        bind_rows(
  //          # attached attributes to these data_gids
  //          d_att %>% filter(gid %in% (dm0 %>% pull(attr_gid)))
  //        )
  //      this_group_info <- this_group_info %>% distinct(row, col) %>% mutate(gid = "dummy")
  //      combined_boundary <- get_group_id_boundary(this_group_info)
  //      this_region_data <- whole_data %>%
  //        filter(type!="empty") %>%
  //        filter(
  //          row <= combined_boundary$r_max,
  //          row >= combined_boundary$r_min,
  //          col <= combined_boundary$c_max,
  //          col >= combined_boundary$c_min
  //        )
  //      this_region_data_rest <- this_region_data %>%
  //        anti_join(this_group_info, by = c("row", "col")) %>%
  //        filter(type %in% c("value", "attribute"))
  //
  //      if (nrow(this_region_data_rest) > 0) {
  //        return(FALSE)
  //      }
  //
  //    }
  
}

bool is_attachable_single(std::string gid1, std::string gid2,
                          DataFrame d_dat, DataFrame d_att, DataFrame data_attr_map,
                          DataFrame whole_data) {
  
  //  ########## logic #########
  //  #### should have similar major sides (of attributes)
  
  CharacterVector attr_group = data_attr_map["attr_group"];
  CharacterVector data_gid = data_attr_map["data_gid"];
  CharacterVector direction = data_attr_map["direction"];
  
  CharacterVector major_str = {"major"}, gid1r = gid1, gid2r = gid2;
  
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
  NumericVector d_dat_row= d_dat["row"];
  NumericVector d_dat_col= d_dat["col"];
  int nr_d_dat = d_dat_row.length();
  LogicalVector d_dat_row_sel = (d_dat_gid == rep(gid1r, nr_d_dat)) | (d_dat_gid == rep(gid2r, nr_d_dat));
  NumericVector d_dat_row_filt = d_dat_row[d_dat_row_sel];
  NumericVector d_dat_col_filt = d_dat_col[d_dat_row_sel];
  CharacterVector d_dat_gid_filt = rep(gid1r, d_dat_row_filt.length());
  
  DataFrame d_com = DataFrame::create(_("row") = d_dat_row_filt,
                                      _("col") = d_dat_col_filt,
                                      _("gid")= d_dat_gid_filt);
  
  
  
  if(!is_attachable_single_logic_2_sub(d_com, whole_data)){
    return false;
  }
  
  
  //  ########## logic #########
  //  #### should have no other entry within the enclosed combined boundary (direction-wise)
  //  #### should have no other entry (non empty) within the enclosed combined boundary attaching major attributes (direction-wise)
  //
  //  data_attr_map_this <- data_attr_map %>% filter(data_gid %in% c(gid1, gid2))
  //
  
  //
  //  if(any(!chks)){
  //    return(FALSE)
  //  }
  
  
  return true;
}

//' is_attachable for multiple rows of gid1 gid2 composition
//'
//' @param g1g2df a df having 'gid1' 'gid2' columns
//' @keywords internal
// [[Rcpp::export]]
LogicalVector is_attachable_multiple(DataFrame g1g2df,
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
