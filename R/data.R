#' NCAA Men's basketball data
#'
#' Collected and cleaned NCAA men's 2023-24 regular season basketball data. This data is
#' ready to be prepared for ranking using a `ranking_table_prep_*()` function. The data
#' is at the team-game level. That is, there are two rows per game: one per team.
#'
#' @format ## `ncaam`
#' A data frame with 10,654 rows and 7 columns:
#' \describe{
#'   \item{team}{The team representing this row of data}
#'   \item{date}{The game date}
#'   \item{home_away}{Whether the `team` is home, away, or something else. NA represents
#'     something else}
#'   \item{opp}{The `team`'s oppponent in this game}
#'   \item{result}{The game result from the persepctive of the `team`}
#'   \item{team_score}{The `team`'s score}
#'   \item{opp_score}{The `opp`'s score}
#' }
"ncaam"


#' PAC-12 Men's basketball data
#'
#' Collected and cleaned NCAA men's 2023-24 regular season basketball data for PAC-12
#' intra-conference play only. This data is ready to be prepared for ranking using a
#' `ranking_table_prep_*()` function. The data is at the team-game level. That is, there
#' are two rows per game: one per team.
#'
#' @format ## `pac12`
#' A data frame with 240 rows and 7 columns:
#' \describe{
#'   \item{team}{The team representing this row of data}
#'   \item{date}{The game date}
#'   \item{home_away}{Whether the `team` is home, away, or something else. NA represents
#'     something else}
#'   \item{opp}{The `team`'s oppponent in this game}
#'   \item{result}{The game result from the persepctive of the `team`}
#'   \item{team_score}{The `team`'s score}
#'   \item{opp_score}{The `opp`'s score}
#' }
"pac12"


#' NCAA Men's basketball data rankings
#'
#' Collected and cleaned AP rankings for NCAA men's basketball following the 2023-24
#' regular season basketball data.
#'
#' @format ## `ap`
#' A data frame with 25 rows and 2 columns:
#' \describe{
#'   \item{team}{The NCAA men's basketball team}
#'   \item{ap_rank}{The AP rank following the 2023-24 regular season}
#' }
"ap"
