



# for extra_functionalities

ef <- list()

ef$task1_inst_github <- function(){
  remotes::install_github("jimhester/archive")
}

ef$task2_archive_extract <- function(){
  f <- function(src, dest){
    archive::archive_extract(src, dest)
  }
  f
}

ef$task3_explore_it_plot <- function(){
  f <- function(x, method = c("collapsibleTree","DiagrammeR"), ...){
    method <- match.arg(method)
    if(method == "collapsibleTree"){
      if(!tidycells:::is_available("collapsibleTree") & tidycells:::is_available("DiagrammeR")){
        method <- "DiagrammeR"
      }
    }
    if(method == "DiagrammeR"){
      if(!tidycells:::is_available("DiagrammeR") & tidycells:::is_available("collapsibleTree")){
        method <- "collapsibleTree"
      }
    }
    if(method == "collapsibleTree"){
      tidycells:::get_well_name_maps(ex, only_well_names = T) %>% collapsibleTree::collapsibleTreeNetwork()
    }else{
      to_e_n_df <- function(x){
        ens <- tidycells:::get_well_name_maps(ex, only_well_names = T)
        all_names <- ens$well_name_parent %>% c(ens$well_name_child) %>% unique()
        all_names <- all_names[!is.na(all_names)]
        node_df <- tibble::tibble(id = seq_along(all_names), label =  all_names, type = "a", style = "filled", color = "aqua", shape = "rectangle")
        edf <- ens %>% dplyr::filter(!is.na(well_name_parent), !is.na(well_name_child))
        edge_df <- node_df %>% dplyr::select(id, label) %>% 
          dplyr::right_join(edf, by = c("label" = "well_name_parent")) %>% dplyr::select(from = id, well_name_child) %>% 
          dplyr::left_join(node_df %>% dplyr::select(id, label), by = c("well_name_child"= "label")) %>% 
          dplyr::select(from, to = id) %>% dplyr::mutate(id = seq_along(from), rel="a")
        node_df <- node_df %>% dplyr::mutate(label = basename(label))
        list(n = node_df, e = edge_df)
      }
      
      plot_DiagrammeR <- function(x, ...){
        ne <- to_e_n_df(x)
        DiagrammeR::create_graph(ne$n, ne$e) %>% DiagrammeR::render_graph(...)
      }
      
      plot_DiagrammeR(x, ...)
    }
  }
}



saveRDS(ef, file = "inst/extdata/extra_functionalities.rds", version = 2)
