#' Create Eurostat Source Description
#' 
#' @importFrom eurostat label_eurostat_tables
#' @keywords internal, metadata

create_eurostat_source <- function(code) {
  paste0( eurostat::label_eurostat_tables(code), " [Eurostat]")
}