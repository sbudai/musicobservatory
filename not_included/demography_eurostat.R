#' Download Demographic Data From Eurostat
#' 
#' Download the demographic data from Eurostat.
#' @importFrom dplyr bind_rows left_join select filter mutate
#' @importFrom dplyr distinct_all inner_join
#' @importFrom eurostat get_eurostat label_eurostat get_eurostat_toc
#' @examples
#' \dontrun{national_accounts_data()} 
#' @export 

national_accounts_eurostat <- function() {
  
  toc <- eurostat::get_eurostat_toc()
  
  consumption_raw <- eurostat_download_label('tec00134')

  create_eurostat_source <- function(code) {
    paste0( eurostat::label_eurostat_tables(code), " [Eurostat]")
  }
  
  consumption <- consumption_raw %>%
    filter ( unit == "PC_TOT") %>%
    filter ( coicop_label %in% c("Restaurants and hotels", 
                                 "Recreation and culture")) %>%
    mutate ( indicator_description = paste0("Final household consumption on ", 
                                            tolower(coicop_label), " % of total")) %>%
    mutate ( indicator = paste0('tec00134', '_', unit, "_",  coicop) ) %>%
    mutate ( source = create_eurostat_source('tec00134')) %>%
    select ( -all_of(c("coicop", "coicop_label")) ) %>%
    filter ( time  >= 2008 ) %>%
    mutate ( code = 'tec00134')
  
  bind_rows ( consumption, consumption_detailed, gdp) %>%
    mutate ( retrieve_date = Sys.Date()) %>%
    left_join ( toc_elements, by = 'code' ) %>%
    dplyr::select (-code )
}


