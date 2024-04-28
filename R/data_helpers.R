#' Subset a data.frame by columns and rows
#'
#' This function takes a data.fram containing a column that lines up to column names, and
#'   filters to where the column are in the selected values, and only shows the column
#'   names matching the selected values.
#'
#' @param data A data.frame
#' @param column_subset The columns + rows to be chosen
#' @param id_col The name of the column containing the values that line up with column
#'   names
#'
#' @return A data.frame
#' @export
view_subset <- function(data, column_subset, id_col = "team") {
  data |>
    dplyr::filter(.data[[id_col]] %in% column_subset) |>
    dplyr::select(!!rlang::sym(id_col), dplyr::all_of(column_subset))
}
