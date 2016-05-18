#' rmdoc: rmarkdown documents for software documentation
#'
#' Provides
#' \itemize{
#'   \item R Markdown template
#'   \item Functionality to extract data model diagrams from databases
#'   \item RStudio AddIn for data model diagrams
#' }
#'
#' @section Setup:
#'
#' Add connection strings for databases in .Renviron file like this:
#'
#' \code{RMDOC_CON_STRING_xxxxx=Driver={SQL Server};Server=ssssss;Database=ddddd;Uid=***;Pwd=***}
#'
#' \code{RMDOC_CON_STRING_yyyyy=Driver={SQL Server};Server=eeeeee;Database=aaaaa;Uid=***;Pwd=***}
#'
#' where xxxxx and yyyyyy will be your "logical" database name.
#'
#' @section Usage with rmarkdown:
#'
#' Add parameters to your rmd file yaml header (which database is used by default):
#'
#' \code{
#'  params:
#'
#'    database: xxxxx
#'  }
#'
#' Include the following knitr chunk before your content:
#'
#' \preformatted{
#'  ```{r database_setup, cache=TRUE}
#'  if(!is.null(params$database)) {
#'    .rmdoc_db_meta <- get_db_meta(database = params$database)
#'    .rmdoc_db_meta_desc <- get_db_meta_desc(database = params$database)
#'    save(list = c(".rmdoc_db_meta", ".rmdoc_db_meta_desc"), file = "data/db_meta.Rdata")
#'  }
#'  ```
#' }


#' Draw graphviz diagrams with \code{display_graph()} functions
#'
#' Draw datamodel diagrams with \code{display_graph(get_graph_from_yaml())} functions
#'
#' @docType package
#' @name rmdoc-package
#' @aliases rmdoc
NULL
