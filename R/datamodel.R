# Functions for:
# 1. Reading YAML with existing tables (reverse engineering) and new tables in yaml definition
# 2. Cache reverse engineered datamodel

#' Get DB connecton string
#'
#' Get environment variable RMDPC_CON_STRING_[DatabaseName]
#'
#' @param database Logical name of the database
#' @return Connection string
#' @export
#' @keywords internal
get_db_connection_string <- function(database) {
  # get_db_connection_string("BasicDev")
  env_var <- paste0("RMDOC_CON_STRING_", database)
  con_string <- Sys.getenv(env_var)
  if(con_string == "") {
    stop("Environment variable ", env_var,
         " is empty. To start R session with the initialized environvent variable
         add it in .Renviron file")
  }
  con_string
}



#' Get meta data from database
#'
#' Executes SQL query on system tables to get information about database datamodel.
#' The result is used by \code{\link{get_db_model}} to extract a data model object.
#'
#' @param database Logical name of the database
#' @param query_str Query to execute (by default the predefined query is used)
#' @param con_string Connection string - by default it is read from
#'   environment variable named RMDOC_CON_STRING_[database]
#' @return A data frame with all columns and related information
#' @export
get_db_meta <- function(database = NULL, query_str = NULL, con_string = NULL) {
  if(missing(con_string)) {
    if(missing(database)) {
      stop("Missing database or con_string parameter.")
    }
    con_string <- get_db_connection_string(database)
  }
  if(is.null(con_string) || con_string == "") {
    stop("No connection string defined for database ", database)
  }
  if(missing(query_str)) {
    sql_file <- system.file("sql/database_re.sql", package = "rmdoc")
    query_str <- paste( readLines(sql_file), collapse = "\n" )
  }
  con <- RODBC::odbcDriverConnect(con_string)
  dm_re <- RODBC::sqlQuery(con, query_str, stringsAsFactors = FALSE, errors=TRUE)
  RODBC::odbcClose(con)

  dm_re$type_desc <-
    paste0(
      dm_re$type,
      ifelse(dm_re$type %in% c("decimal", "varchar"), paste0(" ", dm_re$max_length), ""),
      ifelse(dm_re$type %in% c("decimal" ), paste0(",",  dm_re$precision), ""),
      ifelse(dm_re$type %in% c("decimal" ), paste0(",",  dm_re$scale), "")
    )

  dm_re
}

#' Get table descriptions from database
#'
#' Executes SQL query on system tables to get information about tables.
#'
#' @param database Logical name of the database
#' @param query_str Query to execute (by default the predefined query is used)
#' @param con_string Connection string - by default it is read from
#'   environment variable named RMDOC_CON_STRING_[database]
#' @return A data frame with all columns and related information
#' @export
get_db_meta_desc <- function(database = NULL, query_str = NULL, con_string = NULL) {
  if(missing(con_string)) {
    if(missing(database)) {
      stop("Missing database or con_string parameter.")
    }
    con_string <- get_db_connection_string(database)
  }
  if(is.null(con_string) || con_string == "") {
    stop("No connection string defined for database ", database)
  }
  if(missing(query_str)) {
    sql_file <- system.file("sql/database_re_desc.sql", package = "rmdoc")
    query_str <- paste( readLines(sql_file), collapse = "\n" )
  }
  con <- RODBC::odbcDriverConnect(con_string)
  dm_re <- RODBC::sqlQuery(con, query_str, stringsAsFactors = FALSE, errors=TRUE)
  RODBC::odbcClose(con)

  dm_re
}

#' Get data model object from db meta
#'
#' @param table_filter A list with table names (tables = ...)
#' @param db_meta All database meta data (see get_db_meta)
#' @export
get_db_model <- function(table_filter = NULL, db_meta = NULL) {

  if(missing(db_meta)) {
    db_meta <- get0(".rmdoc_db_meta")
  }
  if(!missing(table_filter)) {
    db_meta <- db_meta[db_meta$table %in% unlist(table_filter), ]
  }
  if(is.null(db_meta) || nrow(db_meta) == 0) {
    return(NULL)
  }

  dm <-  datamodelr::as.data_model(db_meta)
  # deduplicate column names
  # dm$columns <-
  #   dm$columns[!duplicated(dm$columns[, c("table", "column")] ),]

  # add segments
  for(seg in names(table_filter)) {
    dm$tables$segment[dm$tables$table %in% table_filter[[seg]]] <- seg
  }
  dm
}

#' Get data model from yaml definition
#'
#' Combine tables from YAML definition (table: elements) and
#' reverse engineered model (extract_to: elements)
#'
#' @param x YAML string with data model definition
#' @param db_meta Database meta data
#' @return Data model object
#' @export
get_dm_from_yaml <- function(x, db_meta = NULL) {

  if(missing(db_meta)) {
    db_meta <- get0(".rmdoc_db_meta")
  }

  # if it looks like a yaml file name, just read the file
  if( grepl("\\.ya?ml$", tolower(x) ) ) {
    x <- paste(readLines(x, warn = FALSE), collapse = "\n")
  }
  ret <- ""
  try({
    dm <- NULL
    yml <- yaml::yaml.load(x)
    if(any(sapply(yml, function(x) !is.null(x[["table"]])))) {
      dm <- datamodelr::dm_read_yaml(text = x)
    }
    table_filter <- .get_extract_list(yml)
    if(length(table_filter) > 0) {
      dm_re <- get_db_model(table_filter = table_filter, db_meta = db_meta)
      if(!is.null(dm)) {
        dm <- merge_dm(dm_re, dm)
      } else {
        dm <- dm_re
      }
    }
  })
  dm
}

#' Get graph from data model
#'
#' @param dm Datamodel object
#' @param display_options Graph display options (see \code{\link{graph_display_options}})
#' @return graphviz dot string
#' @export
get_graph_from_dm <- function(dm, display_options = NULL) {

  if(missing(display_options) || is.null(display_options)) {
    display_options <- graph_display_options()
  }

  ret <- ""
  try({
    if(is.null(dm)) { return("") }

    max_cols <- as.numeric(display_options$max_cols)
    if(!missing(max_cols) && max_cols > 0 && display_options$view_type == "all") {
      dm$columns$order1 <-
        unlist(lapply(rle(dm$columns$table)$lengths, function(x) 1:x))
      dm$columns <- dm$columns[dm$columns$order1 <= max_cols,]
    }

    if(!display_options$outside_ref) {
      dm$references <- dm$references[dm$references$ref %in% dm$tables$table, ]
    }
    col_attr <- c("column")
    if(display_options$show_types && !is.null(dm$columns[["type_desc"]])) {
      col_attr <- c(col_attr, "type_desc")
    }
    if(display_options$show_descriptions && !is.null(dm$columns[["description"]])) {
      col_attr <- c(col_attr, "description")
    }
    graph <- datamodelr::dm_create_graph(
      dm,
      rankdir = display_options$rankdir,
      view_type = display_options$view_type,
      col_attr = col_attr
    )
    ret <- graph$dot_code
  })
  ret

}

#' Get graph from YAML
#'
#' @param x YAML string or .yaml file name
#' @param display_options Graph display options (see \code{\link{graph_display_options}})
#' @param db_meta Database meta data
#' @export
get_graph_from_yaml <- function(x, display_options = NULL, db_meta = NULL) {

  if(missing(display_options) || is.null(display_options)) {
    display_options <- graph_display_options()
  }

  if(missing(db_meta)) {
    db_meta <- get0(".rmdoc_db_meta")
  }

  # if it looks like a yaml file name, just read the file
  if( grepl("\\.ya?ml$", tolower(x) ) ) {
    x <- paste(readLines(x, warn = FALSE), collapse = "\n")
  }

  ret <- ""
  try({
    dm <- get_dm_from_yaml(x = x, db_meta = db_meta)
    if(is.null(dm)) {
      return("")
    }
    ret <-
      get_graph_from_dm(dm = dm, display_options)

  })
  ret
}



.get_extract_list <- function(x) {

  x_seg_re <- x[sapply(x, function(x) !is.null(x[["extract_to"]]))]
  re_list <-
    lapply(x_seg_re, function(x) {
      x$tables
    })

  if(is.null(re_list) ||
     length(re_list) == 0 ||
     is.null(re_list[[1]] ))
  {
    return(NULL)
  }

  names(re_list) <- sapply(x_seg_re, function(x) x$extract_to)
  return(re_list)
}


.merge_df <- function(x, y, ...) {
  if(length(x) == 0 || nrow(x) == 0) return(y)
  if(length(y) == 0 || nrow(y) == 0) return(x)
  for(n in names(x)) {
    if(is.null(y[[n]])) y[[n]] <- NA
  }
  for(n in names(y)) {
    if(is.null(x[[n]])) x[[n]] <- NA
  }
  rbind(x, y)
}

merge_dm <- function(x, y) {
  # merge two data model objects
  # (using .merge_df instead of base::merge to keep original order)
  dm_new <- list()

  dm_new$tables <- .merge_df(x$tables, y$tables)
  dm_new$tables <- dm_new$tables[!duplicated(dm_new$tables$table, fromLast = TRUE),]

  dm_new$columns <- .merge_df(x$columns, y$columns)
  dm_new$columns <- dm_new$columns[!duplicated(dm_new$columns[, c("table","column")], fromLast = TRUE),]

  dm_new$references <- .merge_df(x$references, y$references)
  class(dm_new) <- c("data_model", "list")

  return(dm_new)
}

# get_table_columns <- function(table_name, db_meta = NULL) {
#   if(missing(db_meta)) {
#     if(exists(".rmdoc_db_meta") && !is.null(.rmdoc_db_meta)) {
#       db_meta <- get(".rmdoc_db_meta")
#     }
#   }
#   ret <- db_meta[ db_meta$table == table_name, ]
#   ret <- datamodelr::as.data_model(ret)[["columns"]]
# }

#' Display graph with \code{DiagrammeR::grViz} (using knitr sizing)
#'
#' @param diagram spec for a diagram as either text, filename string, or file connection.
#' @param engine string for the Graphviz layout engine
#' @param allow_subst a boolean that enables/disables subsitution functionality
#' @param options parameters supplied to the htmlwidgets framework
#' @param width width
#' @param height height
#' @export
display_graph <- function (diagram = "", engine = "dot", allow_subst = TRUE, options = NULL,
                    width = NULL, height = NULL)
{
  knit_format <- knitr::opts_knit$get('rmarkdown.pandoc.to')
  if(is.null(knit_format)) knit_format <- "html"

  if(!interactive() && knit_format != "html") {
    if(missing(width)) {
      width <- 1000
    }
    if(missing(height)) {
      w1 <- knitr::opts_current$get("fig.width")
      h1 <- knitr::opts_current$get("fig.height")
      if(!is.null(w1) && !is.null(h1))
      height <- width * h1 / w1
    }
    #stop("Je ok: ", w1, ",", h1, " -- ", width, ",", height)
  }
  DiagrammeR::grViz(
    diagram = diagram,
    engine = engine,
    allow_subst = allow_subst,
    options = options,
    width = width,
    height = height
  )
}

#' Graph display options
#'
#' Graph display options used in get_graph_from_yaml or get_graph_from_dm
#'
#' @param rankdir A graphviz rankdir option ("BT", "RL", "TB", "LR")
#' @param view_type Can be "all" (by default), "keys_only" or "title_only". It defines the level of details for the table rendering (all columns, only primary and foreign keys or no columns)
#' @param outside_ref Include tables only referenced but not specified in diagram definition
#' @param show_types Show type description of a column
#' @param show_descriptions Show descriptions
#' @param max_cols Max columns displayed (when view_type == "all")
#' @export
graph_display_options <- function(
  rankdir = "RL",
  view_type = "all",
  outside_ref = FALSE,
  show_types = FALSE,
  show_descriptions = FALSE,
  max_cols = 30
) {

  params <- as.list(match.call())[-1]
  defaults <- as.list(formals(graph_display_options))
  l1 <- c(params, defaults)
  l1[!duplicated(names(l1))]
}

#' Run app
#'
#' @export
run_diagram_app <- function() {
    shiny::runApp(system.file("app", package = "rmdoc"))
}

#' Get Table Description
#'
#' Prints table description markdown
#'
#' @param tables Vector of table names
#' @param db_meta Database meta data
#' @param db_meta_desc Database meta data with table descriptions
#' @export
get_table_description <- function(tables, db_meta = NULL, db_meta_desc = NULL) {

  if(missing(db_meta)) {
    db_meta <- get0(".rmdoc_db_meta")
  }
  if(missing(db_meta_desc)) {
    db_meta_desc <- get0(".rmdoc_db_meta_desc")
  }

  for(tab_name in tables) {

    columns <- db_meta[db_meta$table == tab_name, ]
    columns[columns$key == 1, "column"] <-
      sprintf("**%s**", columns[columns$key == 1, "column"])
    columns[!is.na(columns$ref), "column"] <-
      sprintf("%s ^%s^",
              columns[!is.na(columns$ref), "column"],
              columns[!is.na(columns$ref), "ref"]
      )

    columns <- columns[, c(
      "column", "description", "mandatory", "type_desc")
      ]

    columns[is.na(columns)] <-  ""
    names(columns) <- c("Column", "Description", "M", "Type")

    table_description <-
      db_meta_desc$table_desc[db_meta_desc$name == tab_name]

    table_description[is.na(table_description)] <- ""

    table_md <-
      knitr::kable(format = "markdown",
                   columns, row.names = FALSE, caption = tab_name, align = "l"
      )

    table_md <- c(sprintf("\n###Table %s\n\n%s\n\n", tab_name, table_description), table_md)
    attr(table_md, "format") <- "markdown"
    class(table_md) <- "knitr_kable"
    print(table_md)
  }
}

#' Print Diagram Description
#'
#' @param diagram_file A yaml file with diagram definition
#' @param db_meta Database meta data
#' @param db_meta_desc Database meta data with table descriptions
#' @export
print_diagram_description <- function(diagram_file, db_meta = NULL, db_meta_desc = NULL) {
  if(missing(db_meta)) {
    db_meta <- get0(".rmdoc_db_meta")
  }
  if(missing(db_meta_desc)) {
    db_meta_desc <- get0(".rmdoc_db_meta_desc")
  }

  file_name <- diagram_file
  dm <- get_dm_from_yaml(file_name, db_meta = db_meta)

  tables <- sort(dm$tables$table)
  get_table_description(tables, db_meta = db_meta, db_meta_desc = db_meta_desc)
}
