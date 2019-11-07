#' Sample NFL Play-by-Play Data
#'
#' This function will return a sample play from the nflscrapR play-by-play
#' data for a given down, distance, yards from the team's goal, and a
#' particular strategy.
#'
#'
#' @param what_down 1st, 2nd, 3rd, or 4th down
#' @param yards_to_go How many yards to go until a first down or TD
#' @param yards_from_own_goal The number of yards from the possession team's own goal
#' @param play_by_play_data A data file from nflscrapR
#' @param situation A character string denoting the usual strategy ("normal") or always go for it in fourth down ("go for it fourth")
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
#'             play_by_play_data = reg_pbp_2018,
#'             situation = "normal")
#' }
sample_play <- function(what_down,
                        yards_to_go,
                        yards_from_own_goal,
                        play_by_play_data,
                        situation = "go for it fourth"){
  down_original <- what_down
  if (yards_from_own_goal <= 5){yards_from_own_goal <- 5}
  if (yards_from_own_goal > 90){yards_to_go = 100 - yards_from_own_goal}
  if (yards_to_go >= 20){yards_to_go <- 20}
  if(situation == "go for it fourth"){
    if(what_down != 1){
      tmp <- dplyr::filter(play_by_play_data,
                           !is.na(yfog),
                           play_type %in% c("pass", "run"),
                           down == what_down | what_down - 1,
                           ydstogo == yards_to_go,
                           yfog == yards_from_own_goal) %>%
        dplyr::sample_n(size = 1)
    }
    tmp <- dplyr::filter(play_by_play_data,
                         !is.na(yfog),
                         play_type %in% c("pass", "run"),
                         down == what_down,
                         ydstogo == yards_to_go,
                         yfog == yards_from_own_goal) %>%
      dplyr::sample_n(size = 1)
    if(nrow(tmp) == 0 & what_down != 1){ #In case no plays exist
      tmp <- dplyr::filter(play_by_play_data, # e.g. 4th and 1 from own 20 (run/pass)
                           !is.na(yfog),
                           play_type %in% c("pass", "run"),
                           down == what_down -1, # make the down be (what_down - 1)here
                           ydstogo == yards_to_go,
                           yfog == yards_from_own_goal) %>%
        dplyr::sample_n(size = 1)
    } else if(nrow(tmp) == 0 & what_down == 1) {
      tmp <- dplyr::filter(play_by_play_data, # in case there are no rows and its 1st down
                           !is.na(yfog),
                           play_type %in% c("pass", "run"),
                           down == what_down + 1,
                           ydstogo == yards_to_go,
                           yfog == yards_from_own_goal) %>%
        dplyr::sample_n(size = 1)
    }
  } else if(situation == "normal") {
    if((dplyr::filter(play_by_play_data,
                      !is.na(yfog),
                      !play_type %in% c("NA", "no_play", "qb_kneel", "qb_spike"),
                      down == what_down,
                      ydstogo == yards_to_go,
                      yfog == yards_from_own_goal) %>%
        dplyr::sample_n(size = 1) %>%
        nrow())==0){
      yards_to_go = yards_to_go - 1
    }
    tmp <- dplyr::filter(play_by_play_data,
                         !is.na(yfog),
                         !play_type %in% c("NA", "no_play", "qb_kneel", "qb_spike"),
                         down == what_down,
                         ydstogo == yards_to_go,
                         yfog == yards_from_own_goal) %>%
      dplyr::sample_n(size = 1)
  }

  yards_to_go <- tmp$ydstogo
  yard_line <- tmp$yardline_100
  yards_gained <- tmp$yards_gained
  yards_from_own_goal <- tmp$yfog

  if(identical(numeric(0), tmp$punt_attempt)){
    tmp$punt_attempt <- 0
  }

  if(tmp$punt_attempt==1){
    if(tmp$punt_blocked == 1){
      new_yfog <- yards_from_own_goal
    } else if(!is.na(tmp$touchback) & (tmp$touchback != 0)){
      new_yfog <- 75
    } else{
      new_yfog <- yards_from_own_goal + tmp$kick_distance
    }
  } else{
    new_yfog <- yards_from_own_goal + yards_gained
  }

  new_yfog <- yards_from_own_goal + yards_gained
  new_distance <- ifelse(yards_gained >= yards_to_go & new_yfog <= 90,
                         10,
                         ifelse(yards_gained >= yards_to_go & new_yfog > 90,
                                100-new_yfog,
                                yards_to_go - yards_gained))
  is.turnover <- tmp$is.turnover
  is.td.offense <- tmp$is.td.offense
  is.field_goal <- tmp$is.field.goal
  play <- tmp$desc
  new_yard_line <- yard_line - yards_gained
  new_down <- ifelse(yards_gained >= yards_to_go,
                     1,
                     down_original + 1)
  end_drive <- {new_down > 4 | is.turnover | is.td.offense | is.field_goal}
  turnover_on_downs <- {down_original == 4 & new_down > 4 &
      !is.turnover & ! is.td.offense}
  kick_dist <- tmp$kick_distance
  if(is.td.offense==1){
    points <- 7
  } #think about 7
  else if(is.field_goal==1){
    points <- 3
  }
  else(points <- 0)
  #if(!is.td.offense & !is.field_goal){points <- 0}
  return(
    dplyr::tibble(
                  down_original = down_original,
                  yards_to_go = yards_to_go,
                  yard_line = yard_line,
                  yards_gained = yards_gained,
                  new_down = new_down,
                  new_distance = new_distance,
                  is.turnover = is.turnover,
                  is.td.offense = is.td.offense,
                  is.field_goal = is.field_goal,
                  end_drive = end_drive,
                  new_yfog = new_yfog,
                  turnover_on_downs = turnover_on_downs,
                  play = play,
                  kick_dist = kick_dist,
                  points = points,
                  new_yard_line = new_yard_line
    )
  )
}
