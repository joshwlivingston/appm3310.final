#' Read NCAA basketball data
#'
#' Reads googlesheet containing collected data from
#'   [sports-reference.com](https://stathead.com/basketball/cbb/team-game-finder.cgi?request=1&comp_type=reg&game_status=1&order_by=date&match=team_game&year_max=2024&order_by_asc=1&timeframe=seasons&comp_id=NCAAM&year_min=2024).
#'
#' @param league The league ID corresponding to the sheet name. Currently, only "NCAAM"
#'   is supported.
#'
#' @details
#' Calling this function directly will cause you to authenticate using your google sheets
#'   account. You may also access the raw data at `ncaam_raw`
#'
#'
#' @return A tibble
#' @export
read_ncaa_data <- function(league) {
  if (league != "NCAAM") stop(sprintf("Input `league` must be 'NCAAM' not %s", league))
  googlesheets4::read_sheet(googlesheet_url(), sheet = league, skip = 1)
}


#' Clean NCAA data
#'
#' Cleans NCAA data that was read using [read_ncaa_data()]
#'
#' @param ncaa_data A tibble returned from [read_ncaa_data()]
#'
#' @return A tibble with team, date, and game result columns
#'
#' @importFrom rlang .data
#' @export
clean_ncaa_data <- function(ncaa_data) {
  data_out <-
    ncaa_data |>
    dplyr::rename(
      id = .data$Rk
      ,team = .data$Team
      ,date = .data$Date
      ,home_away = .data$`...4`
      ,opp = .data$Opp
      ,result_score = .data$Result
    ) |>
    dplyr::mutate(opp_is_in_team = vapply(.data$opp, function(x) x %in% .data$team, logical(1))) |>
    dplyr::filter(.data$opp_is_in_team) |>
    dplyr::select(-"opp_is_in_team") |>
    dplyr::filter(.data$id != "Rk") |>
    dplyr::select(-"id") |>
    dplyr::mutate(
      ,date = purrr::reduce(.data$date, c) |> as.Date()
      ,home_away = dplyr::case_when(
        is.na(.data$home_away) ~ "home"
        ,.data$home_away == "@" ~ "away"
        ,TRUE ~ NA_character_
      )
      ,result = stringr::str_sub(.data$result_score, start = 1, end = 1)
      ,scores = purrr::map(.data$result_score, extract_scores)
      ,team_score = purrr::map_chr(.data$scores, extract_first)
      ,opp_score = purrr::map_chr(.data$scores, extract_second)
      ,dplyr::across(c("team_score", "opp_score"), as.integer)
    ) |>
    dplyr::select(-"result_score" ,-"scores") |>
    dplyr::as_tibble()

  return(data_out)
}


#' Convert to matrix
#'
#' @param tbl a table resulting from one of the `ranking_table_*()` family of functions
#'
#' @return A square matrix with named rows
#' @export
to_matrix <- function(tbl) {
  row_names <- tbl$team

  mat <-
    tbl |>
    dplyr::select(-"team") |>
    as.matrix(dimnames = list(row_names)) |>
    check_is_square()

  return(mat)
}


googlesheet_url <- function()
  "https://docs.google.com/spreadsheets/d/1-KL_Ib_YSkrnA24nWCGpmx8Y35xYIG9VmKAH71I3ObU"
