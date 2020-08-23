#' Create Eurostat Source Description
#' 
#' @importFrom eurostat label_eurostat_tables
#' @keywords internal
 
create_eurostat_source <- function(code) {
  paste0( eurostat::label_eurostat_tables(code), " [Eurostat]")
}


eurostat_download_label <- function (code) {
  
  tmp <- eurostat::get_eurostat(code, time_format = "num") 
  join_by_vars <- names(tmp)[! names(tmp) %in% c("time", "values", "geo")]
  
  tmp %>% left_join ( eurostat_create_metadata_information(tmp), by = join_by_vars )
}

#' @importFrom dplyr select distinct_all left_join bind_cols
#' @importFrom eurostat label_eurostat
#' @importFrom purrr set_names
 
eurostat_create_metadata_information <- function (dat) {
    
    unique_metadata <- dat %>%
      select ( -all_of(c("values", "time", "geo")) ) %>%
      distinct_all()
    
    unique_metadata %>% bind_cols( 
      unique_metadata %>% 
        eurostat::label_eurostat() %>%
        purrr::set_names (paste0(names(.), "_label")))
    
}
