sample_drives_until_score <- function(n_drives,
                                      from_yard_line = 25,
                                      play_by_play_data,
                                      FUN1 = sample_play,
                                      FUN2 = sample_play_go_for_it){
  all_drives <- NULL
  for (i in 1:n_drives) {
    current_drive <- NULL
    new_yfog <- from_yard_line
    new_down <- 1
    new_distance <- 10
    end_of_drive <- FALSE
    play_num <- 1
    is_td_offense <- FALSE
    is_field_goal <- FALSE
    turnover_on_downs <- FALSE
    is_turnover <- FALSE
    drive_counter <- 1
    while (!is_td_offense & !is_field_goal) {
      if(new_distance > 100 - new_yfog){
        new_distance <- 100 - new_yfog
      }
      cat(sprintf("Current Sim, Drive, Down: %s %s %s\n",
                  i, drive_counter, new_down))
      if((drive_counter %% 2) == 1){
        play <- down_distance_updater(new_down,
                                      new_distance,
                                      new_yfog,
                                      play_by_play_data,
                                      FUN = FUN1)
      } else {
        play <- down_distance_updater(new_down,
                                      new_distance,
                                      new_yfog,
                                      play_by_play_data,
                                      FUN = FUN2)
      }
      play$drive_counter <- drive_counter
      play$play_num <- play_num
      current_drive <- rbind(current_drive, play)
      new_down <- play$new_down
      new_distance <- play$new_distance
      new_yfog <- play$new_yfog
      end_of_drive <- play$end_drive
      play_num <- play_num + 1
      is_td_offense <- play$is_td_offense
      is_field_goal <- play$is_field_goal
      if(end_of_drive){
        new_down <- 1
        new_distance <- 10
        new_yfog <- 100 - from_yard_line
        end_of_drive <- FALSE
        play_num <- 1
        drive_counter <- drive_counter + 1
      }
    }
    current_drive$sim <- i
    i <- i + 1
    all_drives <- rbind(all_drives, current_drive)
  }
  return(all_drives)
}
