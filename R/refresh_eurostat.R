#' Refresh Eurostat indicators
#'
#' Refresh them. 
#'  
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate filter select left_join 
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{national_accounts_data()} 
#' @export 

refresh_eurostat <- function() {
  
  data ( "eurostat_datasets", package = "musicobservatory", 
         envir = environment())
  
  indicators <- eurostat_datasets
  
  ## this is not the final code, it will only refresh the new ones
  
  population <- get_eurostat("demo_r_d2jan")
  
  newborn <- population %>% filter ( age == "Y_LT1", 
                                     sex == "T", 
                                     unit == "NR", 
                                     # filter technical non-territorial units
                                     values > 0) %>%
    mutate ( indicator = "population_newborn")  %>%
    select ( -age )
  
  total <- population %>% filter ( age == "TOTAL", 
                                   sex == "T", 
                                   unit == "NR", 
                                   values > 0)  %>%
    mutate ( indicator = "population_total") %>%
    select ( -age )
  
  
  newborn_rate <- newborn %>%
    tidyr::pivot_wider ( names_from = "indicator", 
                         values_from = "values") %>%
    left_join (  total %>%
                   tidyr::pivot_wider ( names_from = "indicator", 
                                        values_from = "values") , 
                 by = c("unit", "sex",  "geo", "time")
    ) %>%
    mutate ( values = population_newborn / population_total, 
             unit = "PCT", 
             indicator = "newborn_rate")
  
}