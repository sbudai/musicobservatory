#' Download National Accounts Data From Eurostat
#' 
#' Download the national accounts data from Eurostat.
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
  
  consumption_detailed_raw <- eurostat_download_label('nama_10_co3_p3')
 
  consumption_detailed_metadata <- consumption_detailed_raw %>%
    dplyr::select ( -all_of(c("values", "time", "geo"))) %>%
    distinct_all() %>%
    filter ( coicop %in% c("CP11", "CP09", "CP091", "CP092", "CP093", "CP094", "CP082"), 
             unit %in% c("CP_EUR_HAB", "CP_MEUR", "PD10_EUR")) %>%
    mutate ( indicator_description = paste0("Final household consumption on ", 
                                            tolower(coicop_label), unit_label)) %>%
    mutate ( indicator = paste0('nama_10_co3_p3', '_', unit, "_",  coicop) ) %>%
    mutate ( source = create_eurostat_source('nama_10_co3_p3'))  %>%
    mutate ( code = 'nama_10_co3_p3')
    
  
  consumption_detailed <-  consumption_detailed_raw  %>%
    inner_join ( consumption_detailed_metadata, 
                 by = c("unit", "coicop", "unit_label", "coicop_label")) %>%
    dplyr::select ( -all_of(c("coicop", "coicop_label")) ) %>%
    filter ( time  >= 2008 )
  
  gdp_raw <- eurostat_download_label('tec00001')
  
  gdp_metadata <- gdp_raw %>%
    dplyr::select ( -all_of(c("values", "time", "geo"))) %>%
    distinct_all() %>%
    mutate ( indicator_description = paste0(
      na_item_label, " [", tolower(unit_label), "]"), 
      indicator = paste0('tec00001', '_', unit, "_",  na_item)
    ) %>%
    mutate ( source = create_eurostat_source('tec00001')) %>%
    mutate ( code = 'tec00001')
    
  gdp <- gdp_raw %>%
    inner_join ( gdp_metadata, 
                 by = c("na_item", "unit", "na_item_label", "unit_label")) %>%
    dplyr::select ( -all_of(c("na_item", "na_item_label"))) %>%
    dplyr::filter ( time >= 2008 ) 
  
  toc_elements <- toc %>%
    filter ( code %in% c("tec00001", "nama_10_co3_p3", "tec00134")) %>%
    dplyr::select ( all_of(c("code", "last update of data")) ) %>%
    purrr::set_names ( c("code", "last_update_at_source"))
  
  bind_rows ( consumption, consumption_detailed, gdp) %>%
    mutate ( retrieve_date = Sys.Date()) %>%
    left_join ( toc_elements, by = 'code' ) %>%
    dplyr::select (-code )
}


