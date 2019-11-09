down_distance_updater <- function(what_down,
                                  yards_to_go,
                                  yards_from_own_goal,
                                  pbp_data,
                                  FUN = sample_play){
  # down_original <- what_down
  if (yards_from_own_goal <= 5){yards_from_own_goal <- 5}
  if (yards_from_own_goal > 90){yards_to_go = 100 - yards_from_own_goal}
  if (yards_to_go >= 20){yards_to_go <- 20}
  play <- FUN(what_down,
              yards_to_go,
              yards_from_own_goal,
              pbp_data)

  yards_to_go <- play$ydstogo
  yard_line <- play$yardline_100
  yards_gained <- play$yards_gained
  yards_from_own_goal <- play$yfog

  if(identical(numeric(0), play$punt_attempt)){
    play$punt_attempt <- 0
  }

  if(play$punt_attempt==1){
    if(play$punt_blocked == 1){
      new_yfog <- yards_from_own_goal
    } else if(!is.na(play$touchback) & (play$touchback != 0)){
      new_yfog <- 75
    } else{
      new_yfog <- yards_from_own_goal + play$kick_distance
    }
  } else{
    new_yfog <- yards_from_own_goal + yards_gained
  }

  # new_yfog <- yards_from_own_goal + yards_gained
  new_distance <- ifelse(yards_gained >= yards_to_go & new_yfog <= 90,
                         10,
                         ifelse(yards_gained >= yards_to_go & new_yfog > 90,
                                100 - new_yfog,
                                yards_to_go - yards_gained))
  # is_turnover <- play$is_turnover
  # is_td_offense <- play$is.td.offense
  # is.field_goal <- play$is.field.goal
  play <- play$desc
  new_yard_line <- yard_line - yards_gained
  new_down <- ifelse(yards_gained >= yards_to_go,
                     1,
                     down_original + 1)
  end_drive <- {new_down > 4 | play$is_turnover | play$is_td_offense |
      play$is_field_goal}
  turnover_on_downs <- {down_original == 4 & new_down > 4 &
      !play$is_turnover & !play$is_td_offense}
  # kick_dist <- play$kick_distance
  if(play$is_td_offense == 1){
    points <- 7 #think about 7
  } else if(play$is_field_goal == 1){
    points <- 3
  }
  else {points <- 0}
  #if(!is.td.offense & !is.field_goal){points <- 0}
  return(
    data.frame(
      down_original = what_down,
      yards_to_go = yards_to_go,
      yard_line = yard_line,
      yards_gained = yards_gained,
      new_down = new_down,
      new_distance = new_distance,
      is_turnover = is_turnover,
      is_td_offense = is_td_offense,
      is_field_goal = is_field_goal,
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
