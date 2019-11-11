#' Sample NFL Play-by-Play Data
#'
#' This function will return a sample play from the nflscrapR play-by-play
#' data for a given down, distance, yards from the team's goal, using an
#' overly agressive strategy for going for it.
#'
#' @param what_down 1st, 2nd, 3rd, or 4th down
#' @param yards_to_go How many yards to go until a first down or TD
#' @param yards_from_own_goal The number of yards from the possession team's own goal
#' @param play_by_play_data A data file from nflscrapR
#'
#' @return A tibble containing lots of info
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sample_play_go_for_it(what_down = 3,
#'                       yards_to_go = 2,
#'                       yards_from_own_goal = 45,
#'                       play_by_play_data = reg_pbp_2018)
#' }
sample_play_go_for_it <- function(what_down,
                                  yards_to_go,
                                  yards_from_own_goal,
                                  play_by_play_data){
  if(what_down != 1){
    play <- play_by_play_data[!is.na(yfog) &
                                play_type %in% c("pass", "run") &
                                (down == what_down | what_down - 1) &
                                ydstogo == yards_to_go &
                                yfog == yards_from_own_goal, ][
                                  sample(1:.N, size = 1), ]
  }
  play <- play_by_play_data[!is.na(yfog) &
                              play_type %in% c("pass", "run") &
                              down == what_down &
                              ydstogo == yards_to_go &
                              yfog == yards_from_own_goal, ][
                                sample(1:.N, size = 1), ]

  if(nrow(play) == 0 & what_down != 1){ #In case no plays exist
    play <- play_by_play_data[!is.na(yfog) &
                                play_type %in% c("pass", "run") &
                                down == what_down - 1 &
                                ydstogo == yards_to_go &
                                yfog == yards_from_own_goal, ][
                                  sample(1:.N, size = 1), ]

  } else if(nrow(play) == 0 & what_down == 1) {
    play <- play_by_play_data[!is.na(yfog) &
                                play_type %in% c("pass", "run") &
                                down == what_down + 1 &
                                ydstogo == yards_to_go &
                                yfog == yards_from_own_goal, ][
                                  sample(1:.N, size = 1), ]

  }
  return(play)
}
