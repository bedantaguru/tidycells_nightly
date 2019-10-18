
source("00_nightly_only/jsplumb/jsplumb_plugin.R")

require(DiagrammeR)


x <- suppressMessages( readr::read_csv("00_nightly_only/jsplumb/lucidchart.csv"))


node_df <- x %>%
  filter(!is.na(`Shape Library`)) %>%
  select(id =Id, label = `Text Area 1`) %>%
  mutate(type = "a", style = "filled", color = "aqua", shape = "rectangle")


edge_df <- x %>%
  filter(Name=="Line") %>%
  mutate(alt_dest = !str_detect(`Destination Arrow`, "Arrow")) %>%
  mutate(from = ifelse(alt_dest, `Line Destination`, `Line Source`),
         to = ifelse(alt_dest, `Line Source`, `Line Destination`)) %>%
  select(id = Id, from, to) %>% mutate(rel = "a")



node_df %>% mutate(label = as.character(id)) %>% create_graph(edge_df) %>% render_graph(layout = "tree")

cb <- code_base_gen(node_df, edge_df)

ic <- jsplumb_injection("docs/articles/FileTypes.html", proto_loc = "00_nightly_only/jsplumb/proto", cb)

#browseURL(ic$tloc)
ic$copy()
