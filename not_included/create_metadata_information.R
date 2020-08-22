#' Create Metadata Information Columns For Table
#' @param dat A eurostat table
#' @importFrom eurostat label_eurostat
#' @importFrom purrr set_names
#' @importFrom dplyr select distinct_all bind_cols
#' @importFrom tidyselect all_of
#' @keywords internal
#' @return A eurostat data table with original Eurostat data codes and 
#' data labels from the appropriate Eurostat metadata library.

create_metadata_information <- function (dat) {
  
  unique_metadata <- dat %>%
    select ( -all_of(c("values", "time", "geo")) ) %>%
    distinct_all()
  
  unique_metadata %>% bind_cols( 
    unique_metadata %>% 
    eurostat::label_eurostat() %>%
    purrr::set_names (paste0(names(.), "_label")))

}
