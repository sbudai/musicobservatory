#' Indicator for Music Observatory
#'
#' The class inherits all methods from a data frame, but has
#' many fixed attributes.
#'
#' @param x A data.frame or tibble.
#' @param  shortcode A short, unique, programatically usable indicator
#' ID.
#' @param description A precise character string describing the indicator 
#' for the data catalogue of the data observatory.
#' @param date_created The creation day or the refreshment day of the indicator.
#' @param date_earliest The date of the earliest observation. Can be a numeric containing a year.
#' @param date_latest The date of the latest observation.
#' @param original_source Defaults to {NA_character_}.
#' @param original_code The identifier in the original source, if applicable.
#' @param doi The digital object identifier (obtained from a provider)
#' @param doi_url The URL which points to the uploaded object (in the doi provider inventory)
#' @param keyword1 The first keywords, must be one of the pillars of the 
#' observatory.
#' @param keyword2 The second keyword must be a topic within a pillar.
#' @param keyword3 A search term within the topic.
#' @param keywords A character vector of any optional, further keywords.
#' @rdname indicator
#' @return A data.frame or tibble with indicator attributes.
#' @importFrom dplyr distinct_all
#' @importFrom assertthat assert_that is.date
#' @examples
#' test_indicator <- indicator (
#'                      x <- data.frame ( 
#'                      geo = rep(c("NL", "BE", "LU"), 4), 
#'                      time = rep(c(2016:2019),3), 
#'                      values = runif(12, 1,100)
#'                      ),
#'    shortcode = "observatory_test_1", 
#'    description = "A test indicator with random numbers", 
#'    date_created = as.Date ( "2020-08-24"),
#'    date_earliest  = min (x$time, na.rm=TRUE),
#'    date_latest  =  max(x$time, na.rm=TRUE),
#'    doi = NA_character_,
#'    doi_url = NA_character_,
#'    keyword1 = "test",  keyword2 = "random",  keyword3 = "Benelux"
#' )
#' 
#' ## Only the first 10 observations are printed 
#' str( test_indicator )

#' @export

indicator <- function(x, 
                      shortcode, 
                      description,
                      date_created, 
                      date_earliest,
                      date_latest,
                      original_source = NA_character_,
                      original_code = NA_character_,
                      doi = NA_character_,
                      doi_url = NA_character_,
                      keyword1, 
                      keyword2, 
                      keyword3, 
                      keywords = NA_character_ ) {
  
  assertthat::assert_that(is.data.frame(x))
  assertthat::assert_that(assertthat::is.date(date_created))
  assertthat::assert_that(nchar(shortcode)>1)
  assertthat::assert_that(nchar(keyword1)>1)
  assertthat::assert_that(nchar(keyword2)>1)
  assertthat::assert_that(nchar(keyword3)>1)
  assertthat::assert_that(nchar(description)>3)
  
  if ( length(keywords) == 1 &  is.na(keywords[1])) {
    keywords <- c(keyword1, keyword2, keyword3)
  } else {
    keywords <- c(keyword1, keyword2, keyword3, keywords)
  }
  
  if (is.na(original_source)) {
    original_source <- "music.dataobservatory.eu"
  }
  
  return( new_indicator( x               = x, 
                         shortcode       = shortcode, 
                         description     = description,
                         date_created    = date_created, 
                         date_earliest   = date_earliest,
                         date_latest     = date_latest, 
                         original_source = "music.dataobservatory.eu",
                         original_code   = original_code,
                         doi             = doi,
                         doi_url         = doi_url,
                         keyword1        = keyword1, 
                         keyword2        = keyword2, 
                         keyword3        = keyword3, 
                         keywords        = keywords ) )
  
}


#' @rdname indicator
#' @export
is.indicator <- function (x) inherits(x, "indicator")

#' @rdname indicator
#' @export
print.indicator <- function(x, ... ) {
  
  cat(paste0("indicator [", attr(x, "shortcode"), "] ", 
  paste( c( attr(x, "keyword1"), 
            attr(x, "keyword2"), 
            attr(x, "keyword3")
            ), 
            collapse = " - "), 
  "\n", attr(x, "description")))
  
  n_observations <- attr(x, "observations") 
  
  if ( n_observations > 10 ) {
    cat (paste0("\nThe first 10 observations of ", n_observations, "\n" ))
    print(head(as.data.frame(x),10))
  } else {
    print(as.data.frame(x))
  }
}

## not exported

new_indicator <- function(x, 
                          shortcode, 
                          description,
                          date_created, 
                          date_earliest,
                          date_latest, 
                          observations, 
                          original_source = "music.dataobservatory.eu",
                          original_code = NA_character_,
                          doi = NA_character_,
                          doi_url = NA_character_,
                          keyword1, 
                          keyword2, 
                          keyword3, 
                          keywords = NA_character_ ) {
  
  ## This is an internal function for creating an object of 
  ## class indicator.
  
  
  if ( is.na(original_code) ) original_code <- shortcode
  
  indicator <- x
  attr(indicator, "shortcode") <- shortcode
  attr(indicator, "description") <- description
  attr(indicator, "date_created") <- date_created
  attr(indicator, "date_earliest") <- date_earliest
  attr(indicator, "date_latest") <- date_latest
  attr(indicator, "observations") <- nrow(dplyr::distinct_all ( x ))
  
  attr(indicator, "doi") <- doi
  attr(indicator, "doi_url") <- doi_url
  attr(indicator, "keyword1") <- keyword1
  attr(indicator, "keyword2") <- keyword2
  attr(indicator, "keyword3") <- keyword3
  attr(indicator, "keywords") <- keywords
  
  attr(indicator, "observatory") <-  'music.dataobservatory.eu'
  
  class(indicator) <- c("indicator", class(indicator) ) 
  
  return(indicator)
}
