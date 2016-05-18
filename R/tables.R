
#' Get Excel table
#'
#' @param file File name
#' @param sheet Sheet name
#' @export
get_excel_table <- function(file, sheet) {
  table1 <- readxl::read_excel(path = file, sheet = sheet)
  table1[is.na(table1)] <- ""
  table1
  #knitr::kable(table1, align = "l", caption = caption)
}

#' Get database data
#'
#' @param db_name Logical database name
#' @param query SQL query string
#' @export
get_database_data <- function(db_name, query ) {
  con <- RODBC::odbcDriverConnect(get_db_connection_string(db_name))
  df <- RODBC::sqlQuery(con, query)
  RODBC::odbcClose(con)
  df
}

