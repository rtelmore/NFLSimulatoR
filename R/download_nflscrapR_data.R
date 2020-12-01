#' Download raw nflscrapR data in csv format 
#'
#' This function will return a data.frame after downloading the original file 
#' from the nflscrapR-data website.
#'
#' @param year A year from 2009 to 2019
#' @param type A character string specifying "regular", "pre", or "post", for 
#' regular, pre, or post season, respectively.
#' 
#' @return A data.frame containing play-by-play information from NFL games
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- download_nflscrapR_data("regular", 2019)
#' }

download_nflscrapR_data <- function(type = "regular", year){
  if(type == "regular") {
    abbv <- "reg"
  } else abbv <- type
  url <- paste("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/",
               type, "_season/", 
               abbv, "_pbp_", 
               year, ".csv", 
               sep = "")
  df <- utils::read.csv(url)
}