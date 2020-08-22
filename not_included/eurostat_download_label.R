#' Download And Label Eurostat Data
#' 
#' @param code A eurostat statistical product code
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom purrr set_names
#' @importFrom dplyr left_join
#' @return A eurostat data table with original Eurostat data codes and 
#' data labels from the appropriate Eurostat metadata library.
#' @examples
#' de_input_flow <- input_flow_get ( data_table = iotable_get())
#' 
#' conforming_vector_create ( data_table = de_input_flow )
#' @export 


eurostat_download_label <- function (code) {
  
  tmp <- eurostat::get_eurostat(code, time_format = "num") 
  join_by_vars <- names(tmp)[! names(tmp) %in% c("time", "values", "geo")]
  
  tmp %>% left_join ( create_metadata_information(tmp), by = join_by_vars )

  
  
}
