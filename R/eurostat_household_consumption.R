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
    dplyr::filter( unit == "PC_TOT" ) %>%
    dplyr::filter( coicop_label %in% c( "Restaurants and hotels", 
                                        "Recreation and culture" ) ) %>%
    dplyr::mutate( indicator_description = paste( "Final household consumption on", 
                                                  tolower( coicop_label ), "% of total" ) ) %>%
    dplyr::mutate( indicator = paste0( 'tec00134', '_', unit, "_",  coicop ) ) %>%
    dplyr::mutate( source = create_eurostat_source( 'tec00134' ) ) %>%      # internal function in utils.R
    dplyr::select( -all_of( c( "coicop", "coicop_label") ) ) %>%
    dplyr::filter( time  >= 2008 ) %>%
    dplyr::mutate( code = 'tec00134' )
  
  #distinct ( consumption, indicator, indicator_description )
  
  # setting indicator parameters
  ind_original_source <- "Eurostat"
  ind_original_code   <- "tec00134"
  ind_keyword1        <- "economy"
  ind_keyword2        <- "demand"
  ind_keyword3        <- "driver"
  ind_keywords        <- c( "households", "expenditures", "culture", "recreation" )
  
  
  # FigShare authentication is needed to be done via an interactive session using fs_auth() function call being in the 
  # non-interactive project folder! The fs_auth() function call launches a browser with the related web site 
  # where '.httr-oauth' local file usage should be allowed in order to use this package in a non-interactive session.
  
  # searching for "Daniel Antal" authors on FigShare by his name.
  fs_authors    <- suppressMessages( rfigshare::fs_author_search( 'Antal' ) )
  fs_authors    <- fs_authors[lapply( fs_authors, '[[', 'fname' ) %>% grep( 'Daniel|Dániel', ., useBytes = TRUE )]

  # setting FigShare doi parameters
  fs_type       <- 'dataset'
  fs_author_ids <- sapply( fs_authors, '[[', 'id' )
  fs_categories <- c( 'Economics', 'Sociology', 'Art', 'Performing Arts', 'Statistics', 'Entertainment', 'Media Studies' )
  fs_links      <- 'https://music.dataobservatory.eu/'
  fs_visibility <- 'draft'                                 # or 'public', 'private'
  fs_tags       <- base::union( ind_keyword1, ind_keyword2 ) %>%
    base::union( ., ind_keyword3) %>%
    base::union( ., ind_keywords )
  
  # creating a list (vector actually) of indicators from consumption table
  indicator_list   <- unique( consumption$indicator )
  
  # iterating indices of indicator_list thru
  filenames        <- lapply( seq( indicator_list ),
                              function( i ) {
  
    # filtering corresponding records of chosen indicator
    filtered_table   <- consumption %>%
      dplyr::filter( indicator == indicator_list[[i]] )
    
  
    # creating a temporary csv file on disk
    csv_file         <- tempfile( pattern = indicator_list[[i]],
                                  fileext = '.csv' )
  
    # filling up temporary csv file on disk with the filtered tabular data
    data.table::fwrite( x    = filtered_table,
                        file = csv_file,
                        sep  = '|' )
  
    # creating, uploading and publishing temporary csv file from disk to figshare
    # and obtaing digital object identifier
    doi              <- suppressMessages(
                          rfigshare::fs_new_article(title       = indicator_list[[i]],
                                                    description = unique( filtered_table$indicator_description[1] ) %>%
                                                      sort() %>%
                                                      .[1],
                                                    type        = fs_type,
                                                    authors     = fs_author_ids,
                                                    tags        = fs_tags,
                                                    categories  = fs_categories,
                                                    links       = fs_links,
                                                    files       = csv_file,
                                                    visibility  = fs_visibility)
    )
    
    # FigShare need some time to wholly register everything
    Sys.sleep(1)
  
    # removing temporary csv file from disk
    succ             <- file.remove(file = csv_file)
    
    # querying details of digital object
    doi_details      <- suppressMessages(rfigshare::fs_details(article_id = doi))
    
    # picking URL of digital object
    doi_url          <- ifelse(doi_details$status == "Public", doi_details$doi, 'not public yet')
    
    # composing indicator object
    indicator_obj    <- indicator(
      x               = filtered_table,
      shortcode       = indicator_list[[i]],
      description     = unique( filtered_table$indicator_description[1] ) %>%
        sort() %>%
        .[1],
      date_created    = Sys.Date(),
      date_earliest   = min( filtered_table$time, na.rm = TRUE ),
      date_latest     = max( filtered_table$time, na.rm = TRUE ),
      original_source = ind_original_source,
      original_code   = ind_original_code,
      doi             = doi,
      doi_url         = doi_url,
      keyword1        = ind_keyword1,
      keyword2        = ind_keyword2,
      keyword3        = ind_keyword3,
      keywords        = ind_keywords
    )
    
    # composing file name of to be saved indicator object
    filename         <- file.path( path, paste0( tolower( indicator_list[[i]] ), ".rds" ) )
  
    # saving indicator object into an rds file
    saveRDS( indicator_obj, 
             file    = filename,
             version = 2L )
    
    # returning with the file name of the saved object
    return( filename )
    
  })
  
  paste( filenames, 'file has been saved\n' ) %>%
    message()
  
}
