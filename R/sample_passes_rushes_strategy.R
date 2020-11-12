#' Sample NFL play-by-play data with a specified blend of rushing and passing
#' 
#' This function will return a sample play from the nflscrapR play-by-play
#' data for a given down, distance, yards from the team's goal, using
#' a given pass/rush play strategy. The user may choose a value for the 
#' proportion of passing plays to be sampled. Thus one can test strategies
#' in which the team always passes, always runs, or some distribution of the two.
#' This strategy is only intended for downs 1 - 3, and uses an empirical strategy 
#' for fourth downs.
#' This should be primarily used within the `NFLSimulatoR::sample_play()` function.
#'
#' @param what_down The current down (1st, 2nd, 3rd, or 4th down)
#' @param yards_to_go Number of yards to go until a first down or TD
#' @param yards_from_own_goal The number of yards from the possession team's own goal
#' @param window_yards_from_own_goal Precision parameter for "yards_from_own_goal" (a value of 1 means the sampling
#' will occur within plus or minus 1 of the "yards_from_own_goal" value)
#' @param play_by_play_data A data file from nflscrapR prepped using the prep_pbp_data.R function
#' @param prop_passes Proportion of plays that should be pass plays, between 0 and 1, inclusive
#'
#' @return A tibble containing lots of info
#'
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' sample_passes_rushes_strategy(what_down = 3,
#'                               yards_to_go = 2,
#'                               yards_from_own_goal = 45,
#'                               window_yards_from_own_goal = 2,
#'                               play_by_play_data = reg_pbp_2018,
#'                               prop_passes = 0.5)
#' }

sample_passes_rushes_strategy <- function(what_down,
                                          yards_to_go,
                                          yards_from_own_goal,
                                          window_yards_from_own_goal = 1,
                                          play_by_play_data,
                                          prop_passes = 0.5) {
  
  ## Non-standard eval initialization for data.table
  yfog <- play_type <- down <- ydstogo <- NULL
  
  if(what_down == 4){play <- play_by_play_data[!is.na(yfog) &
                                                 !play_type %in% c("NA",
                                                                   "no_play",
                                                                   "qb_kneel",
                                                                   "qb_spike") &
                                                 down == what_down &
                                                 ydstogo %in% c(yards_to_go - 1:yards_to_go + 1) &
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
                              ydstogo %in% c(yards_to_go - 1:yards_to_go + 1) &
                              yfog %in% c((yards_from_own_goal - window_yards_from_own_goal):
                                            (yards_from_own_goal + window_yards_from_own_goal)),][sample(1:(.N), size = 1),]
  return(play)
  }
  
  else if (stats::runif(1) < prop_passes) {
    play <- play_by_play_data[!is.na(yfog) &
                                !play_type %in% c("NA",
                                                  "no_play",
                                                  "qb_kneel",
                                                  "qb_spike",
                                                  "run") &
                                down == what_down &
                                ydstogo %in% c(yards_to_go - 1:yards_to_go + 1) &
                                yfog %in% c((yards_from_own_goal - window_yards_from_own_goal):
                                              (yards_from_own_goal + window_yards_from_own_goal)),][sample(1:(.N), size = 1),]
    
    
    if (nrow(play) == 0) {
      yards_to_go = yards_to_go - 1
    }
    play <- play_by_play_data[!is.na(yfog) &
                                !play_type %in% c("NA",
                                                  "no_play",
                                                  "qb_kneel",
                                                  "qb_spike",
                                                  "run") &
                                down == what_down &
                                ydstogo %in% c(yards_to_go - 1:yards_to_go + 1) &
                                yfog %in% c((yards_from_own_goal - window_yards_from_own_goal):
                                              (yards_from_own_goal + window_yards_from_own_goal)),][sample(1:(.N), size = 1),]
    
    return(play)
  }
  #Sample rush play
  else {
    play <- play_by_play_data[!is.na(yfog) &
                                !play_type %in% c("NA",
                                                  "no_play",
                                                  "qb_kneel",
                                                  "qb_spike",
                                                  "pass") &
                                down == what_down &
                                ydstogo %in% c(yards_to_go - 1:yards_to_go + 1) &
                                yfog %in% c((yards_from_own_goal - window_yards_from_own_goal):
                                              (yards_from_own_goal + window_yards_from_own_goal)),][sample(1:(.N), size = 1),]
    
    if (nrow(play) == 0 & what_down == 1) {
    play <- play_by_play_data[!is.na(yfog) &
                                !play_type %in% c("NA",
                                                  "no_play",
                                                  "qb_kneel",
                                                  "qb_spike",
                                                  "pass") &
                                down == what_down &
                                ydstogo %in% c(yards_to_go - 1:yards_to_go + 1) &
                                yfog %in% c((yards_from_own_goal - window_yards_from_own_goal):
                                              (yards_from_own_goal + window_yards_from_own_goal)),][sample(1:(.N), size = 1),]
   
    }
    else {
      play <- play_by_play_data[!is.na(yfog) &
                                  !play_type %in% c("NA",
                                                    "no_play",
                                                    "qb_kneel",
                                                    "qb_spike",
                                                    "pass") &
                                  (down == what_down | what_down -1) &
                                  ydstogo %in% c(yards_to_go - 1:yards_to_go + 1) &
                                  yfog %in% c((yards_from_own_goal - window_yards_from_own_goal):
                                                (yards_from_own_goal + window_yards_from_own_goal)),][sample(1:(.N), size = 1),]
      
    }
    return(play)
  }
}

