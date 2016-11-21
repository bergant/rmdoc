
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

#' Get Excel table
#'
#' @param file File name
#' @param skip_empty Skip empty rows
#' @param replace_na Replace NAs with this value
#' @export
get_excel_data <- function(file, skip_empty = TRUE, replace_na = NA) {
  sheets <- readxl::excel_sheets(file)
  stats::setNames(
    lapply(sheets, function(sheet) {
      tab <- readxl::read_excel(file, sheet)
      if(skip_empty) {
        tab <- tab[ stats::complete.cases(tab[, 1]), ]
      }
      tab[is.na(tab)] <- replace_na
      tab
    }),
    sheets
  )
}


#' Get database data
#'
#' @param db_name Logical database name
#' @param query SQL query string
#' @export
get_database_data <- function(db_name, query ) {
  con <- RODBC::odbcDriverConnect(get_db_connection_string(db_name))
  df <- RODBC::sqlQuery(con, query, stringsAsFactors = FALSE)
  RODBC::odbcClose(con)
  df
}

#' Get SQL
#'
#' @param sql_file SQL query file
#' @param ... query parameters
#' @export
get_sql <- function(sql_file, ...) {

  params <- list(...)

  params <-
    lapply(params, function(x) {
      if(is.null(x)) {
        "null"
      } else if(mode(x) == "character"){
        sprintf("'%s'",x)
      } else {
        x
      }
    })

  sql <- paste( readLines(sql_file), collapse = "\n")

  for(param_name in names(params)) {
    sql <- gsub(paste0("declare @", param_name), paste0("--declare @", param_name), sql)
    sql <- gsub(paste0("set @", param_name), paste0("--set @", param_name), sql)
    sql <- gsub(paste0("@", param_name), params[[param_name]], sql)
  }
  sql
}

#' Run SQL
#'
#' @param sql_file SQL query file
#' @param database Logical database name
#' @param ... query parameters
#' @export
run_sql <- function(sql_file, database, ...) {
  sql <- get_sql(sql_file, ...)
  rmdoc::get_database_data(database, sql)
}

#' Get code table data
#'
#' @param code_table A code table name (database table name)
#' @param name_col Cold table field name with codes
#' @param value_col Cold table field name with values
#' @param database Database logical name
#' @return Returns a code table as a list of values
#' @name code_tables
#' @export
get_code_table <- function(code_table, name_col, value_col, database) {
  query = sprintf("select %s, %s from %s", name_col, value_col, code_table)
  ct_data <- rmdoc::get_database_data(database, query)
  as.list(stats::setNames(ct_data[,value_col], ct_data[,name_col]))
}

#' Get code table data
#'
#' Returns code table as a list of values
#'
#' @param code_tables A list of code tables with elements: code_table, name_col,
#'   value_col
#' @rdname code_tables
#' @export
get_code_tables <- function(code_tables, database) {

  adinsure_ct <- lapply(code_tables, function(x) {
    x$database = database
    do.call(get_code_table, args = x)
  })
}
