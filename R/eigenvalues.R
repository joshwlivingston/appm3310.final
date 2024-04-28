#' Compute eigenvalues via power method
#'
#' Given a matrix and iteration parameters, uses the power method to compute eigenvalues.
#'   Before running the power method, checks that matrix is square with non-negative
#'   values, in accordance with the Perron-Frobenius theorem.
#'
#' @param mat A square non-negative matrix
#' @param iterations Maximum number of iterations to perform (default 1000)
#' @param convergance Convergence threshold (default 1e-10)
#'
#' @return A list containing two tibbles: the named eigenvectors and the results from the
#'   iteration
#'
#' @seealso [rankings_from_eigenvalues()], [plot_lambdas()]
#'
#' @export
eigenvalues_power_method <- function(mat,
                                     iterations = 1000,
                                     convergance = 1e-10) {
  # matrix must be square to compute eigenvalues
  check_is_square(mat)
  check_nonnegative_matrix(mat)

  team_names <- dimnames(mat)[[2]]
  ## store matrix dimensions
  m <- nrow(mat)
  n <- ncol(mat)
  ## initialize eigenvector with 1's
  ev <- rep(1, m)

  old_lamdba <- 0
  lambdas <- vector("numeric")
  ## iterate the power method for specified iterations
  for (i in 1:iterations) {
    r_new <- mat %*% ev
    r_normalized <- r_new / sqrt(sum(r_new^2))
    new_lambda <- t(r_normalized) %*% mat %*% r_normalized

    new_lambda_inf <- identical(new_lambda[1, 1], Inf)
    converged <- abs(new_lambda - old_lamdba) < convergance
    cond <- new_lambda_inf | converged
    if (cond) break

    old_lamdba <- new_lambda[1, 1]
    lambdas[i] <- old_lamdba
    ev <- r_new
  }

  lambda_table <- tibble::tibble(
    iter = 1:length(lambdas),
    lambda = lambdas
  )

  ev <- ev[, 1]
  names(ev) <- team_names
  ev_table <- tibble::enframe(ev, "team", "eigenvector")
  return(list(eigenvector_table = ev_table, eigenvalue_table = lambda_table))
}


#' Compute rankings from eigenvalues
#'
#' Takes a vector of eigenvalues and returns data.frame of eigenvalues and corresponding
#'   ranks
#'
#' @param eigenvalues The result of [eigenvalues_power_method()]
#' @param name_col Column to store names of `evs`, if applicable
#'
#' @return A tibble containing the eigenvalues, rank, and a name column, if `evs` is
#'   named
#'
#' @importFrom rlang .data
#' @export
rankings_from_eigenvalues <- function(eigenvalues, name_col = "team") {
  rankings_tbl <-
    eigenvalues$eigenvector_table |>
    dplyr::mutate(rank = rank(-.data$eigenvector)) |>
    dplyr::arrange(.data$rank)
  return(rankings_tbl)
}


#' Plot lambdas from iteration
#'
#' We can use this function to view the results of teh power method. When we view this
#' plot we should see our lambda converging.
#'
#' @param eigenvalues The result of [eigenvalues_power_method()]
#'
#' @return A ggplot2 plot
#' @importFrom rlang .data
#' @export
plot_lambdas <- function(eigenvalues) {
  eigenvalues$eigenvalue_table |>
    ggplot2::ggplot() +
    ggplot2::aes(.data$iter, .data$lambda) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal()
}
