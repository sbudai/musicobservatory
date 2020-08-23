#' Example Downloader
#' 
#' Download the demographic data from Eurostat.
#' @importFrom dplyr select filter mutate
#' @importFrom eurostat get_eurostat label_eurostat get_eurostat_toc
#' @examples
#' \dontrun{national_accounts_data()} 
#' @export 

eurostat_household_consumption <- function( path ) {
  
  toc <- eurostat::get_eurostat_toc()
  
  consumption_raw <- eurostat_download_label('tec00134')  #internal function in utils

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
    mutate ( source = create_eurostat_source('tec00134')) %>%   #internal function in utils
    select ( -all_of(c("coicop", "coicop_label")) ) %>%
    filter ( time  >= 2008 ) %>%
    mutate ( code = 'tec00134')
  
  #distinct ( consumption, indicator, indicator_description )
  
  cp09 <- consumption %>%
    filter ( indicator == "tec00134_PC_TOT_CP09")
  
  cp11 <- consumption %>%
    filter ( indicator == "tec00134_PC_TOT_CP11")
  
  tec00134_PC_TOT_CP09 <- indicator (
    x = cp09,
    shortcode =  "tec00134_PC_TOT_CP09",
    description = cp09$indicator_description[1],
    date_created = Sys.Date(),
    date_earliest  = min (cp09$time, na.rm=TRUE),
    date_latest  =  max(cp09$time, na.rm=TRUE),
    original_source = "Eurostat",
    original_code = "tec00134",
    keyword1 = "economy",
    keyword2 = "demand",
    keyword3 = "driver",
    keywords = c("households", "expenditures", "culture", "recreation")
  )
  
  filename <- file.path(path, tolower("tec00134_PC_TOT_CP09.rds"))
  message ( filename ) 
  saveRDS(tec00134_PC_TOT_CP09, 
          file = filename, version = 2)
  
  tec00134_PC_TOT_CP11 <- indicator (
    x = cp11,
    shortcode =  "tec00134_PC_TOT_CP11",
    description = cp11$indicator_description[1],
    date_created = Sys.Date(),
    date_earliest  = min (cp11$time, na.rm=TRUE),
    date_latest  =  max(cp11$time, na.rm=TRUE),
    original_source = "Eurostat",
    original_code = "tec00134",
    keyword1 = "economy",
    keyword2 = "demand",
    keyword3 = "driver",
    keywords = c("households", "expenditures", "culture", "recreation")
  )
  
  filename <- file.path(path, tolower("tec00134_PC_TOT_CP11.rds"))
  message ( filename ) 
  saveRDS(tec00134_PC_TOT_CP11, 
          file = filename, version = 2)
  
}


