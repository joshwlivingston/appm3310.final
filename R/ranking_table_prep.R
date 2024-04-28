#' Ranking table preparation
#'
#' Given cleaned NCAA data, compute a ranking table using various methods
#'
#' @param ncaa_data Cleaned NCAA data returned from [clean_ncaa_data()]
#'
#' @details `ranking_table_direct_win_loss()` assigns a 1 for each victory, a 0.5 for
#'   each tie, and a 0 for loss, then adds the results
#'
#' @return A ranking table ready to be passed to [to_matrix()]
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
