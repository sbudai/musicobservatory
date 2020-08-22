## code to prepare `eurostat_datasets` dataset goes here

eurostat_datasets <- data.frame ( 
  eurostat_code  = c("tec00134", "demo_r_d2jan"), 
  pillar = c("demand", "demand"), 
  topic = c("macroeconomy", "population")
  )

usethis::use_data(eurostat_datasets, overwrite = TRUE)


