#' Add necessary columns to nflscrapR data
#'
#' @param data An nflscrapR or nflfastR data set. Note that stringsAsFactors = FALSE is assumed.
#'
#' @return a data.table object
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- prep_pbp_data(nflscrapr_pbp_data)
#' }
prep_pbp_data <- function(data){
  
  ## Non-standard eval initialization for data.table
  game_id <- total_home_score <- total_away_score <- desc <- fumble_recovery_1_team <- NULL
  rush_touchdown <- pass_touchdown <- field_goal_attempt <- field_goal_result <- down <- NULL
  yardline_100 <- punt_attempt <- is_fumble <- posteam <- yards_gained <- is_two_point <- NULL

  
  dt <- data.table::setDT(data)[
    , ":="
    (p_diff = total_home_score - total_away_score,
      is_two_point = grepl("TWO-POINT CONVERSION", desc),
      is_fumble = !is.na(fumble_recovery_1_team),
      is_td_offense = rush_touchdown | pass_touchdown,
      is_field_goal = field_goal_attempt & (field_goal_result == "made"),
      yfog = 100 - yardline_100,
      touchback = ifelse(grepl("Touchback", desc), 1, 0),
      punt_attempt = as.numeric(punt_attempt))][
        , ":=" (is_turnover = grepl("INTERCEPTED", desc) |
                  (is_fumble & fumble_recovery_1_team != posteam))][
                    !is.na(yards_gained)][
                      is_two_point == FALSE][
                        !is.na(punt_attempt)]
  dt[, c("next_play_id") := shift(.SD, 1), by = game_id]
  dt <- dt[down != "NA"]
  return(dt)
}
