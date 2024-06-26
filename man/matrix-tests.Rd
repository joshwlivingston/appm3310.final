% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix_tests.R
\name{check_is_square}
\alias{check_is_square}
\alias{check_nonnegative_matrix}
\title{Matrix tests}
\usage{
check_is_square(x)

check_nonnegative_matrix(x)
}
\arguments{
\item{x}{A matrix}
}
\description{
Tests to perform on a matrix to ensure it can have eigenvalues calculated via the
power method
}
\details{
\code{check_is_square()} throws an error if the matrix two dimensions are not
equal \cr
\code{check_non_negative_matrix()} throws an error if any value in the matrix is < 0
}
