#' Sample one NFL play according to some strategy
#'
#' This function will return a sample play from the nflscrapR play-by-play
#' data for a given down, distance, yards from the team's goal, using the
#' usual NFL-coaching strategy.
#'
#' @param what_down The current down (1st, 2nd, 3rd, or 4th down)
#' @param yards_to_go Number of yards to go until a first down or TD
#' @param yards_from_own_goal The number of yards from the possession team's own goal
#' @param window_yards_from_own_goal Precision parameter for "yards_from_own_goal" (a value of 1 means the sampling
#' will occur within plus or minus 1 of the "yards_from_own_goal" value)
#' @param play_by_play_data A data file from nflscrapR prepped using the prep_pbp_data.R function
#' @param strategy A string describing the strategy to be used, default is "normal", others include: "fourth_downs" and "passes_rushes" 
#' which implement some strategy regarding 4th downs and proportion of plays that are passing plays, respectively.
#' @param ... Additional arguments for different strategies
#' 
#' @return A tibble containing lots of info
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sample_play_test(what_down = 3,
#'                  yards_to_go = 2,
#'                  yards_from_own_goal = 45,
#'                  play_by_play_data = pbp_data,
#'                  strategy = "normal")
#' }

sample_play <- function(what_down,
                             yards_to_go,
                             yards_from_own_goal,
                             window_yards_from_own_goal = 1,
                             play_by_play_data,
                             strategy = "normal",
                             ...) {

  ## Non-standard eval initialization for data.table
  yfog <- play_type <- down <- ydstogo <- NULL
  
  # Normal strategy
  if (strategy == "normal") {
    play <- play_by_play_data[!is.na(yfog) &
                                !play_type %in% c("NA",
                                                  "no_play",
                                                  "qb_kneel",
                                                  "qb_spike") &
                                down == what_down &
                                ydstogo == yards_to_go &
                                yfog %in% c((yards_from_own_goal - window_yards_from_own_goal):
                                              (yards_from_own_goal + window_yards_from_own_goal)),][sample(1:(.N), size = 1),]
    
    if (nrow(play) == 0) {
      yards_to_go = yards_to_go - 1
    }
    play <- play_by_play_data[!is.na(yfog) &
                                !play_type %in% c("NA",
                                                  "no_play",
                                                  "qb_kneel",
                                                  "qb_spike") &
                                down == what_down &
                                ydstogo == yards_to_go &
                                yfog %in% c((yards_from_own_goal - window_yards_from_own_goal):
                                              (yards_from_own_goal + window_yards_from_own_goal)),][sample(1:(.N), size = 1),]
   return(play)
    }
  else if (strategy == "passes_rushes") {
      play <- NFLSimulatoR::sample_passes_rushes_strategy(
        what_down = what_down,
        yards_to_go = yards_to_go,
        yards_from_own_goal = yards_from_own_goal,
        play_by_play_data = play_by_play_data,
        ...
      )
    return(play)
  }
  else if (strategy == "fourth_downs") {
    play <- NFLSimulatoR::sample_fourth_down_strategy(
      what_down = what_down,
      yards_to_go = yards_to_go,
      yards_from_own_goal = yards_from_own_goal,
      play_by_play_data = play_by_play_data,
      ...
    )
    return(play)
  }
  
  }
  