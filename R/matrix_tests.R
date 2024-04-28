#' Matrix tests
#'
#' Tests to perform on a matrix to ensure it can have eigenvalues calculated via the
#' power method
#'
#' @param x A matrix
#'
#' @details `check_is_square()` throws an error if the matrix two dimensions are not
#'   equal \cr
#'   `check_non_negative_matrix()` throws an error if any value in the matrix is < 0
#' @rdname matrix-tests
#' @export
check_is_square <- function(x) {
  mat_dim <- dim(x)
  is_square <- do.call(identical, as.list(mat_dim))
  if (isFALSE(is_square)) {
    stop("Matrix is not square.\ndim: %d x %d",
         mat_dim[1], mat_dim[2])
  }
  return(invisible(x))
}

#' @rdname matrix-tests
#' @export
check_nonnegative_matrix <- function(x) {
  is_non_neg <- all(vapply(x, is_nonnegative_vector, logical(1)))
  if (!is_non_neg) stop("Matrix provided does not contain all non-negative entries")
  return(invisible(x))
}

is_nonnegative_vector <- function(vec) all(vec >= 0)
