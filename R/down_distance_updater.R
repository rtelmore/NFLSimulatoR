#' Update the down and distance of a drive
#'
#' The down and distance updater will run a play and update various game-based
#' statistics accordingly.
#'
#' @param what_down The current down (1st, 2nd, 3rd, or 4th down)
#' @param yards_to_go Number of yards to go until a first down or TD
#' @param yards_from_own_goal The number of yards from the possession team's own goal
#' @param play_by_play_data A data file from nflscrapR prepped using the prep_pbp_data.R function
#' @param ... Additional arguments for different strategies
#'
#' @return A data.frame object
#' @export
#'
#' @examples
#' \dontrun{
#' down_distance_updater(what_down = 1,
#'                       yards_to_go = 10,
#'                       yards_from_own_goal = 25,
#'                       play_by_play_data = pbp_data,
#'                       strategy = "normal")
#' } 
#' 
down_distance_updater <- function(what_down,
                                  yards_to_go,
                                  yards_from_own_goal,
                                  play_by_play_data,
                                  ...){
  
  game_id <- play_id <- ydstogo <- down <- NULL
  
  # down_original <- what_down
  if (yards_from_own_goal <= 5){yards_from_own_goal <- 5}
  if (yards_from_own_goal > 90){yards_to_go = 100 - yards_from_own_goal}
  if (yards_to_go >= 20){yards_to_go <- 20}

  play_success <- FALSE
  while(play_success == FALSE){
    play <- NFLSimulatoR::sample_play(what_down,
                yards_to_go,
                yards_from_own_goal,
                play_by_play_data = play_by_play_data,
                ...)
    if(any(is.na(play$desc), identical(play$desc, character(0)))){
      play_success <- FALSE
      yards_from_own_goal <- min(99, yards_from_own_goal + 
                                   sample(c(1, -1, 2, -2), size = 1))
    } else play_success <- TRUE
  }
  
  ## Fix the issue with penalties
  next_play_no_play <- FALSE
  if (play$play_type == "no_play") {
    while (!next_play_no_play) {
      tmp <- play_by_play_data[game_id == play$game_id & 
                                 play_id > play$play_id, ][1, ] 
      play <- tmp
      if (play$play_type != "no_play") {
        next_play_no_play <- TRUE
      }
    }
    ## Update yfog, ydl, etc.
    yards_from_own_goal <- play$yardline_100
    yards_to_go <- ydstogo
    what_down <- down
  }
  
  yard_line <- play$yardline_100
  yards_gained <- play$yards_gained

  if(play$punt_attempt != 0 & !is.na(play$punt_attempt)){
    if(play$punt_blocked == 1){
      new_yfog <- yards_from_own_goal
    } else if(!is.na(play$touchback) & (play$touchback != 0)){
      new_yfog <- 75
    } else{
      if(is.na(play$kick_distance)){
        new_yfog <- 75
      } else {
        new_yfog <- yards_from_own_goal + play$kick_distance
      }
    }
  } else{
    new_yfog <- min(99, yards_from_own_goal + yards_gained)
  }

  new_distance <- ifelse(yards_gained >= yards_to_go & new_yfog <= 90,
                         10,
                         ifelse(yards_gained >= yards_to_go & new_yfog > 90,
                                100 - new_yfog,
                                yards_to_go - yards_gained))
  new_yard_line <- yard_line - yards_gained
  new_down <- ifelse(yards_gained >= yards_to_go,
                     1,
                     what_down + 1)
  end_drive <- {new_down > 4 | play$is_turnover | play$is_td_offense |
      play$is_field_goal}
  turnover_on_downs <- {what_down == 4 & new_down > 4 &
      !play$is_turnover & !play$is_td_offense}
  if(play$is_td_offense == 1){
    points <- 7 #think about 7
  } else if(play$is_field_goal == 1){
    points <- 3
  } else {points <- 0}
  #if(!is.td.offense & !is.field_goal){points <- 0}
  return(
    data.frame(
      down_original = what_down,
      yards_to_go = yards_to_go,
      yard_line = yard_line,
      yards_gained = yards_gained,
      new_down = new_down,
      new_distance = new_distance,
      is_turnover = play$is_turnover,
      is_td_offense = play$is_td_offense,
      is_field_goal = play$is_field_goal,
      end_drive = end_drive,
      new_yfog = new_yfog,
      turnover_on_downs = turnover_on_downs,
      play = as.character(play$desc),
      play_type = play$play_type,
      kick_dist = play$kick_distance,
      points = points,
      new_yard_line = new_yard_line,
      safety = play$safety,
      exp_pts = play$ep,
      exp_pts_added = play$epa,
      stringsAsFactors = FALSE
    )
  )
}

