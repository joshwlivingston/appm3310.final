#' Ranking table preparation
#'
#' Given cleaned NCAA data, compute a ranking table using various methods
#'
#' @param ncaa_data Cleaned NCAA data returned from [clean_ncaa_data()]
#'
#' @details `ranking_table_direct_win_loss()` assigns a 1 for each victory, a 0.5 for
#'   each tie, and a 0 for loss, then adds the results.
#'
#'   `ranking_table_prep_points_scored_mean()` takes the average points scored across all
#'   games between the two teams.
#'
#'   `ranking_table_prep_points_proportion()` takes the
#'   points scored as a percentage of total points in that game.
#'
#'   `ranking_table_prep_points_squared_distance()` takes the difference between the
#'   team's points and the opponent's points and squares the result.
#'
#'   `ranking_table_prep_points_abs_distance()` takes the difference between the
#'   team's points and the opponent's points and takes the absolute value of the result
#'
#' @return A ranking table ready to be passed to [to_matrix()]
#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_direct_win_loss <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      result_num = dplyr::case_when(
        .data$result == "W" ~ 1.0
        ,.data$result == "L" ~ 0.0
        ,TRUE ~ 0.5
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "result_num",
      values_fn = sum,
      values_fill = 0
    )
}

#' @rdname ranking_table_prep
#' @export
ranking_table_prep_points_scored_mean <- function(ncaa_data) {
  ncaa_data |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "team_score",
      values_fn = mean,
      values_fill = 0
    )
}

#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_proportion <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      pct_scored = .data$team_score / .data$team_score + .data$opp_score
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "pct_scored",
      values_fn = mean,
      values_fill = 0
    )
}

#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_squared_distance <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      squared_distance = (.data$team_score - .data$opp_score)^2
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "squared_distance",
      values_fn = mean,
      values_fill = 0
    )
}

#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_abs_distance <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      abs_distance = abs(.data$team_score - .data$opp_score)
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "abs_distance",
      values_fn = mean,
      values_fill = 0
    )
}

#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_sqrt_points <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      sqrt_points = sqrt(.data$team_score)
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "sqrt_points",
      values_fn = mean,
      values_fill = 0
    )
}


#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_polynomial1 <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      polynomial1 = (.data$team_score^3 + .data$team_score^2) / (.data$team_score + .data$opp_score)
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "polynomial1",
      values_fn = mean,
      values_fill = 0
    )
}


#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_polynomial2 <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      polynomial = (.data$team_score^3 + 3*.data$team_score^2) / (.data$team_score + .data$opp_score)^2
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "polynomial",
      values_fn = mean,
      values_fill = 0
    )
}

#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_polynomial3 <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      polynomial = (5 + .data$team_score + .data$team_score^(2 / 3)) / (5 + .data$opp_score + .data$team_score^(2 / 3))
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "polynomial",
      values_fn = mean,
      values_fill = 0
    )
}


#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_polynomial4 <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      linear = (.data$team_score / mean(.data$team_score)) / (.data$opp_score / mean(.data$team_score))
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "linear",
      values_fn = mean,
      values_fill = 0
    )
}

#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_inverse <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      score_to_use = ifelse(.data$result == "W", .data$team_score^2, 1 / .data$team_score)
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "score_to_use",
      values_fn = mean,
      values_fill = 0
    )
}

#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_inverse2 <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      score_to_use = ifelse(.data$result == "W", .data$team_score^2, 1 / .data$team_score)
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "score_to_use",
      values_fn = mean,
      values_fill = 0
    )
}


#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_inverse3 <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      score_to_use = ifelse(.data$result == "W", .data$team_score^3, .data$team_score^(-3))
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "score_to_use",
      values_fn = mean,
      values_fill = 0
    )
}

#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_polynomial5 <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      score_to_use = (100 + .data$team_score - .data$team_score^(1/2)) / (100 + .data$opp_score - .data$opp_score^(1/2))
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "score_to_use",
      values_fn = mean,
      values_fill = 0
    )
}


#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_points_difference_cond <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      score_to_use = ifelse(.data$result == "W", .data$team_score - .data$opp_score, -1 / (.data$team_score - .data$opp_score))
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "score_to_use",
      values_fn = mean,
      values_fill = 0
    )
}

#' @rdname ranking_table_prep
#' @importFrom rlang .data
#' @export
ranking_table_prep_point_run_up <- function(ncaa_data) {
  ncaa_data |>
    dplyr::mutate(
      score_to_use = h((.data$team_score + 1) / (.data$team_score + .data$opp_score + 2))
    ) |>
    tidyr::pivot_wider(
      id_cols = "team",
      names_from = "opp",
      values_from = "score_to_use",
      values_fn = mean,
      values_fill = 0
    )
}

h <- function(x) {
  0.5 + 0.5 * sign(x - 0.5) * sqrt(abs(2 * x - 1))
}
