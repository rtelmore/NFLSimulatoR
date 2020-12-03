#' Download raw nflfastR data in rds format 
#'
#' This function will return a tibble after downloading the original file 
#' from the nflfastR-data website. Note that the tibble will contain all 
#' regular and postseason data.
#'
#' @param year A year from 2009 to 2020
#' 
#' @return A data.frame containing play-by-play information from NFL games
#'
#' @export
#'
#' @examples
#' df <- download_nflfastR_data(2019)

download_nflfastR_data <- function(year){
  web_address <- paste("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",
                       year,
                       ".rds",
                       sep = "")
  df <- readRDS(url(web_address))
  return(df)
}