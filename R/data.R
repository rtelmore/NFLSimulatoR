#' NFL Play-by-Play Data
#'
#' A dataset containing the 256 attributes associated with 452661 plays in the
#' NFL from 2009 regular season through the 2018 regular season. The data were
#' obtained using the nflscrapR package.
#'
#' @format A data frame with 452661 rows and 256 variables:
#' \describe{
#'   \item{play_id}{id within a game}
#'   \item{game_id}{id of the specific game}
#'   \item{home_team}{home team playing}
#'   \item{away_team}{away team playing}
#'   \item{posteam}{team with possession}
#'   ...
#' }
#' @source \url{https://ryurko.github.io/nflscrapR-data/}
"nfl-pbp-df"
