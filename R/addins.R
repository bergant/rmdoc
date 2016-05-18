display_graph_from_yaml_addin <- function() {
  #grViz("digraph a {a -> b}")
  yaml <- paste(
    rstudioapi::getActiveDocumentContext()[["contents"]],
    collapse = "\n")
  if(!exists(".rmdoc_db_meta")) {
    warning("Database meta data not initialised... ")
  }
  graph <- rmdoc::get_graph_from_yaml(yaml)
  DiagrammeR::grViz(graph)
}

