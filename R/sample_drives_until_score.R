#' Sample a Series of Drives Until a Score, a strategy to test verses the normal strategy
#'
#' @param n_sims The number of simulations
#' @param from_yard_line The starting field position (defaults to 25)
#' @param play_by_play_data A data file from nflscrapR prepped using the prep_pbp_data.R function
#' @param strategy "normal", "passes_rushes", or "fourth_downs"
#' @param single_drive TRUE indicates only a single drive
#' @param progress logical for inclusion of a progress bar
#' @param ... Additional arguments for different strategies
#' 
#' @return A data.frame of drives
#'
#' @export
#' @examples
#' \dontrun{
#' sample_drives_until_score(2, 25, dt)
#' }
#'
sample_drives_until_score <- function(n_sims,
                                      from_yard_line = 25,
                                      play_by_play_data,
                                      strategy = "normal",
                                      single_drive = FALSE,
                                      progress = TRUE,
                                      ...){
  if(progress == TRUE){
    pb <- progress::progress_bar$new(
      format = "  Simulations [:bar] :percent eta: :eta",
      total = n_sims, clear = FALSE, width = 60)
  }
  all_drives <- NULL
  for (i in 1:n_sims) {
    if(progress == TRUE){
      pb$tick()
    }
    current_drive <- NULL
    new_yfog <- from_yard_line
    new_down <- 1
    new_distance <- 10
    end_of_drive <- FALSE
    play_num <- 1
    is_td_offense <- FALSE
    is_field_goal <- FALSE
    is_safety <- FALSE
    turnover_on_downs <- FALSE
    is_turnover <- FALSE
    drive_counter <- 1
    while_logic <- TRUE
    while (while_logic) {
      if(new_distance > 100 - new_yfog){
        new_distance <- 100 - new_yfog
      }
      if((drive_counter %% 2) == 1){
        play <- NFLSimulatoR::down_distance_updater(new_down,
                                                    new_distance,
                                                    new_yfog,
                                                    play_by_play_data,
                                                    strategy = strategy,
                                                    ...)
      } else {
        play <- NFLSimulatoR::down_distance_updater(new_down,
                                                    new_distance,
                                                    new_yfog,
                                                    play_by_play_data,
                                                    strategy = "normal",
                                                    ...)
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
      is_safety <- play$safety
      if(end_of_drive | play$safety == 1){
        new_down <- 1
        new_distance <- 10
        new_yfog <- 100 - new_yfog
        end_of_drive <- FALSE
        play_num <- 1
        drive_counter <- drive_counter + 1
      }
      if (single_drive) {
        while_logic <- (!is_td_offense &
          !is_field_goal &
          !is_safety &
          drive_counter == 1)
      } else {
      while_logic <- (!is_td_offense & !is_field_goal & !is_safety)
      }
      
    }
    current_drive$sim <- i
    i <- i + 1
    all_drives <- rbind(all_drives, current_drive)
  }
  return(all_drives)
}
