#' SQL Convert
#'
#' @return
#' A string SQL Query:
#'
#' @param query A character query
#' @param from from query ddl (presto)
#' @param from to query ddl (sparksql)
#'
#' @examples
#' \dontrun{
#' query <- "select * from iris limit 100"
#' from <- "presto"
#' to <- "spqrksql
#' convert_query(query, from, to)
#' }
#'
#' @rdname sql_convert
#' @export
sql_convert <- function(from = 'presto', to = 'sparksql'){
  
  query <- "
  select 
    *
  from aaa
  where 
    created >= to_unixtime(parse_datetime('20190216','yyyyMMdd')) * 1000
  "
  
  check_list <- list()
  if(all(from == 'presto', to == 'sparksql')){
    
    if(stringr::str_detect(query, "to_unixtime")){
      list(title = "to_unixtime -> unix_timestamp", description = "", example = "")
      
      review_cnt <- review_cnt + 1
    }
    
    if(stringr::str_detect(query, "parse_datetime")){
      review_cnt <- review_cnt + 1
    }
    
    cli::cat_rule(center = " * RESULTS * ")
    cli::cat_bullet("Recommended count : ", review_cnt, bullet = "tick")
    cli::cat_bullet("to_unixtime -> unix_timestamp", bullet_col = "green")
    cli::cat_bullet("parse_datetime -> to_date", bullet_col = "green")
    cli::cat_boxx("parse_datetime -> to_date", border_col = "green")
    cli::cat_rule(center = " * END * ")
    
  }
  
}