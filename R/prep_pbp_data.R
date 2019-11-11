#' Add necessary columns to nflscrapR data
#'
#' @param nflscrapR_data An nflscrapR data set. Note that stringsAsFactors = FALSE is assumed.
#'
#' @return a data.table object
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- prep_pbp_data(nflscrapr_pbp_data)
#' }
prep_pbp_data <- function(nflscrapR_data){
  dt <- data.table::setDT(nflscrapR_data)
  dt[, ":="
     (p_diff = total_home_score - total_away_score,
       is_two_point = grepl("TWO-POINT CONVERSION", desc),
       is_fumble = !is.na(fumble_recovery_1_team),
       is_td_offense = rush_touchdown | pass_touchdown,
       is_field_goal = field_goal_attempt & (field_goal_result == "made"),
       yfog = 100 - yardline_100,
       touchback = ifelse(grepl("Touchback", desc), 1, 0),
       punt_attempt = as.numeric(punt_attempt))][
         , ":=" (is_turnover = grepl("INTERCEPTED", desc) |
         (is_fumble & fumble_recovery_1_team != posteam))]
  return(dt[!is.na(yards_gained) & !is_two_point & !is.na(punt_attempt)])
}
