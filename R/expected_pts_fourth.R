#' Decision for 4th downs based on expected points
#'
#' This function will return the expected points for several 
#' 4th down decision. The options are "go for it",
#' "field goal", or "punt".  This should be primarily
#' used within the `NFLSimulatoR::sample_play()` function.
#'
#' @param yards_from_goal The number of yards until a team scores a touchdown
#' @param yards_to_go Number of yards to go until a first down or TD
#' @param play_data A data file from nflscrapR prepped using the prep_pbp_data.R function
#'
#' @return A data.frame of the expected points of three fourth down options
#'
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' expected_pts_fourth(what_down = 1,
#'                       yards_to_go = 10,
#'                       yards_from_own_goal = 25,
#'                       play_by_play_data = reg_pbp_2018)
#' }

expected_pts_fourth <- function(yards_from_goal,
                                yards_to_go,
                                play_data){
  
  ## Non-standard eval initialization for data.table
  opp_yard_line <- yardline_100 <- yards_gained <- play_type <- down <- NULL
  ydstogo <- touchback <- kick_distance <- return_yards <- NULL
  
  if (yards_to_go >= 20){
    yards_to_go <- 20
  }
  
  #go for it expected value
  p_gain <-
    nrow(play_data[yardline_100 %in% c(yards_from_goal, 
                                       yards_from_goal - 1, 
                                       yards_from_goal + 1) &
                     yards_gained >= yards_to_go &
                     play_type %in% c('run', 'pass')]) /
    nrow(play_data[yardline_100 %in% c(yards_from_goal, 
                                       yards_from_goal - 1, 
                                       yards_from_goal + 1) &
                     play_type %in% c('run', 'pass')])
  
  p_stopped <- 1 - p_gain
  
  df_gain <- play_data[yardline_100 %in% c(yards_from_goal - yards_to_go,
                                           yards_from_goal - yards_to_go - 1,
                                           yards_from_goal - yards_to_go + 1) &
                         down == 1 &
                         ydstogo == ifelse((yards_from_goal - yards_to_go) < 10,
                                         yards_from_goal - yards_to_go,
                                         10) &
                         play_type %in% c('run', 'pass')]

  ep_gain <- mean(df_gain$ep, na.rm = T)
  df_stopped <- play_data[yardline_100 %in% c(100 - yards_from_goal,
                                              100 - yards_from_goal - 1,
                                              100- yards_from_goal + 1) &
                            down == 1 &
                            ydstogo == ifelse((100 - yards_from_goal) < 10, 
                                              100 - yards_from_goal, 10)]

  ep_stopped <- -1*mean(df_stopped$ep, na.rm = T)
  ev_goforit <- as.numeric(p_gain * ep_gain + p_stopped * ep_stopped)
  
  # field goal expected value
  df_fg <- play_data[yardline_100 %in% c(yards_from_goal,
                                         yards_from_goal - 1,
                                         yards_from_goal + 1) &
                       play_type == 'field_goal']

  p_make <- mean(df_fg$fg_prob,na.rm = T)
  p_miss <- 1 - p_make
  
  df_miss <- play_data[yardline_100 %in% `if`(yards_from_goal <= 20, 
                                              c(19:21), 
                                              c(yards_from_goal + 18,
                                                yards_from_goal+ 17,
                                                yards_from_goal+ 19)) &
                         down == 1 &
                         ydstogo == 10]
                    
  df_opp_fg <- play_data[yardline_100 == 75 & down == 1 & ydstogo == 10]
  ep_opp_fg <- -1*mean(df_opp_fg$ep, na.rm = T)
  ep_miss <- -1*mean(df_miss$ep, na.rm = T)
  ev_fg <- p_make*3 + ep_opp_fg + p_miss*ep_miss
  
  # punt expected value 
  df_punt <- play_data[yardline_100 %in% c(yards_from_goal, 
                                           yards_from_goal + 1, 
                                           yards_from_goal - 1) &
                         play_type == "punt"]
  df_punt[, opp_yard_line := ifelse(touchback == 1, 
                                    75, 
                                    -1*(yardline_100 - 
                                          kick_distance - 100 + 
                                          return_yards))]
  avg_opp_ydln <- round(mean(df_punt$opp_yard_line, na.rm = TRUE))
  df_punt_opp <- play_data[yardline_100 == avg_opp_ydln &
                             down==1 &
                             ydstogo ==10]
  ev_punt <- -1*mean(df_punt_opp$ep,nna.rm = TRUE)
  
  
  return(
    data.frame(
      ev_goforit = ev_goforit,
      ev_fg = ev_fg,
      ev_punt = ev_punt
    )
  )
}
