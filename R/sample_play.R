#' Sample NFL Play-by-Play Data
#'
#' This function will return a sample play from the nflscrapR play-by-play
#' data for a given down, distance, yards from the team's goal, using the
#' usual NFL-coaching strategy.
#'
#' @param what_down 1st, 2nd, 3rd, or 4th down
#' @param yards_to_go How many yards to go until a first down or TD
#' @param yards_from_own_goal The number of yards from the possession team's own goal
#' @param play_by_play_data A data file from nflscrapR prepped using the prep_pbp_data.R function
#'
#' @return A tibble containing lots of info
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sample_play(what_down = 3,
#'             yards_to_go = 2,
#'             yards_from_own_goal = 45,
#'             play_by_play_data = pbp_data)
#' }
sample_play <- function(what_down,
                        yards_to_go,
                        yards_from_own_goal,
                        play_by_play_data){

  play <- play_by_play_data[!is.na(yfog) &
                              !play_type %in% c("NA",
                                                "no_play",
                                                "qb_kneel",
                                                "qb_spike") &
                              down == what_down &
                              ydstogo == yards_to_go &
                              yfog == yards_from_own_goal, ][
                                sample(1:(.N), size = 1), ]

  if(nrow(play) == 0){
    yards_to_go = yards_to_go - 1
  }
  play <- play_by_play_data[!is.na(yfog) &
                              !play_type %in% c("NA",
                                                "no_play",
                                                "qb_kneel",
                                                "qb_spike") &
                              down == what_down &
                              ydstogo == yards_to_go &
                              yfog == yards_from_own_goal, ][
                                sample(1:.N, size = 1), ]
  return(play)
}
