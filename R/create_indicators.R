#' A temporary wrapper for creating indicators
#' 
#' Download the demographic data from Eurostat.
#' @param path A directory to save the indicators to. If \code{NULL} then
#' creates a \code{tempdir()} to save the data.
#' @return The function does not return a value, only saves the indicators
#' to \code{path}.
#' @examples
#' \dontrun{create_indicators()} 
#' @export 

create_indicators <- function ( path = NULL ) {
  
  if (is.null(path)) {
    new_path = tempdir()
  } else if (dir.exists(path)) {
    new_path <- path
  } else {
    new_path <- tempdir()
    warning(path, " does not exists, saving to ", new_path )
  }
 
eurostat_household_consumption(new_path)
  
}