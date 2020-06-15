#' Sample NFL Play-by-Play Data with a Specific 4th down Strategy
#'
#' This function will return a sample play from the nflscrapR play-by-play
#' data for a given down, distance, yards from the team's goal, using an
#' a given strategy on fourth down. The strategies are: empirical, always 
#' going for it on fourth down, never going for it on fourth down, go for it 
#' if one is less than a certain distance from a first down/touchdown,
#' and go for it if it maximizes one's expected points. This should be primarily
#' used within the `nflsimulator::sample_play()` function.
#'
#' @param what_down The current down (1st, 2nd, 3rd, or 4th down)
#' @param yards_to_go Number of yards to go until a first down or TD
#' @param yards_from_own_goal The number of yards from the possession team's own goal
#' @param play_by_play_data A data file from nflscrapR prepped using the prep_pbp_data.R function
#' @param strat The specific fourth down strategy `empirical`, `always_go_for_it`, `never_go_for_it`,`yds_less_than`,`exp_pts`
#'
#' @return A tibble containing lots of info
#'
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' sample_fourth_down_strategy(what_down = 3,
#'                       yards_to_go = 2,
#'                       yards_from_own_goal = 45,
#'                       play_by_play_data = reg_pbp_2018)
#' }

sample_fourth_down_strategy <- function(what_down,
                                        yards_to_go,
                                        yards_from_own_goal,
                                        play_by_play_data,
                                        strat = "empirical",
                                        ...) {
  if (strat == "empiricial") {
    play <- play_by_play_data[!is.na(yfog) &
                                !play_type %in% c("NA",
                                                  "no_play",
                                                  "qb_kneel",
                                                  "qb_spike") &
                                down == what_down &
                                ydstogo == yards_to_go &
                                yfog == yards_from_own_goal,][sample(1:(.N), size = 1),]
    
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
                                yfog == yards_from_own_goal,][sample(1:.N, size = 1),]
  }
  
  # Always go for it
  #sample a play, if it is 4th down, we always go for it
  if (strat == "always_go_for_it") {
    if (what_down != 1) {
      play <- play_by_play_data[!is.na(yfog) &
                                  play_type %in% c("pass", "run") &
                                  (down == what_down | what_down - 1) &
                                  ydstogo == yards_to_go &
                                  yfog == yards_from_own_goal,][sample(1:.N, size = 1),]
    } else{
      play <- play_by_play_data[!is.na(yfog) &
                                  play_type %in% c("pass", "run") &
                                  down == what_down &
                                  ydstogo == yards_to_go &
                                  yfog == yards_from_own_goal,][sample(1:.N, size = 1),]
    }
    if (nrow(play) == 0 & what_down != 1) {
      #In case no plays exist
      play <- play_by_play_data[!is.na(yfog) &
                                  play_type %in% c("pass", "run") &
                                  down == what_down - 1 &
                                  ydstogo == yards_to_go &
                                  yfog == yards_from_own_goal,][sample(1:.N, size = 1),]
      
    } else if (nrow(play) == 0 & what_down == 1) {
      play <- play_by_play_data[!is.na(yfog) &
                                  play_type %in% c("pass", "run") &
                                  down == what_down + 1 &
                                  ydstogo == yards_to_go &
                                  yfog == yards_from_own_goal,][sample(1:.N, size = 1),]
      
    }
  }
  
  #Never go for it
  #sample a play, if it is 4th down, we never go for it
  if (strat == "never_go_for_it") {
    if (what_down == 4) {
      play <- play_by_play_data[!is.na(yfog) &
                                  !play_type %in% c("pass", "run",
                                                    "no_play", "qb_kneel") &
                                  (down == what_down | what_down - 1) &
                                  ydstogo == yards_to_go &
                                  yfog == yards_from_own_goal,][sample(1:.N, size = 1),]
    } else if (what_down != 1) {
      play <- play_by_play_data[!is.na(yfog) &
                                  (down == what_down | what_down - 1) &
                                  ydstogo == yards_to_go &
                                  yfog == yards_from_own_goal,][sample(1:.N, size = 1),]
    }
    if (nrow(play) == 0 & what_down == 4) {
      #In case no plays exist
      play <- play_by_play_data[!is.na(yfog) &
                                  play_type %in% c("pass", "run") &
                                  down == what_down - 1 &
                                  ydstogo %in% c(yards_to_go - 1, yards_to_go + 1) &
                                  # give a 2 yard range
                                  yfog == yards_from_own_goal,][sample(1:.N, size = 1),]
      
    } else if (nrow(play) == 0 & what_down == 1) {
      play <- play_by_play_data[!is.na(yfog) &
                                  #play_type %in% c("pass", "run") &
                                  down == what_down + 1 &
                                  ydstogo == yards_to_go &
                                  yfog == yards_from_own_goal,][sample(1:.N, size = 1),]
      
    } else if (nrow(play) == 0 &
               what_down != 1) {
      #In case no plays exist
      play <- play_by_play_data[!is.na(yfog) &
                                  #play_type %in% c("pass", "run") &
                                  down == what_down - 1 &
                                  ydstogo == yards_to_go &
                                  yfog == yards_from_own_goal,][sample(1:.N, size = 1),]
      
    }
  }
  
  if (strat == "yds_less_than") {
    if (what_down == 4 &
        yards_to_go >= 5) {
      play <- sample_play(
        what_down = what_down,
        yards_to_go = yards_to_go,
        yards_from_own_goal = yards_from_own_goal,
        play_by_play_data = play_by_play_data
      )
    }
    else{
      if (what_down != 1) {
        play <- play_by_play_data[!is.na(yfog) &
                                    play_type %in% c("pass", "run") &
                                    (down == what_down |
                                       what_down - 1) &
                                    ydstogo == yards_to_go &
                                    yfog == yards_from_own_goal, ][sample(1:.N, size = 1), ]
      } else{
        play <- play_by_play_data[!is.na(yfog) &
                                    play_type %in% c("pass", "run") &
                                    down == what_down &
                                    ydstogo == yards_to_go &
                                    yfog == yards_from_own_goal, ][sample(1:.N, size = 1), ]
      }
      if (nrow(play) == 0 & what_down != 1) {
        #In case no plays exist
        play <- play_by_play_data[!is.na(yfog) &
                                    play_type %in% c("pass", "run") &
                                    down == what_down - 1 &
                                    ydstogo == yards_to_go &
                                    yfog == yards_from_own_goal, ][sample(1:.N, size = 1), ]
        
      } else if (nrow(play) == 0 & what_down == 1) {
        play <- play_by_play_data[!is.na(yfog) &
                                    play_type %in% c("pass", "run") &
                                    down == what_down + 1 &
                                    ydstogo == yards_to_go &
                                    yfog == yards_from_own_goal, ][sample(1:.N, size = 1), ]
        
      }
    }
  }
  return(play)
}
  