% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eigenvalues.R
\name{eigenvalues_power_method}
\alias{eigenvalues_power_method}
\title{Compute eigenvalues via power method}
\usage{
eigenvalues_power_method(mat, iterations = 1000, convergance = 1e-10)
}
\arguments{
\item{mat}{A square non-negative matrix}

\item{iterations}{Maximum number of iterations to perform (default 1000)}

\item{convergance}{Convergence threshold (default 1e-10)}
}
\value{
A list containing two tibbles: the named eigenvectors and the results from the
iteration
}
\description{
Given a matrix and iteration parameters, uses the power method to compute eigenvalues.
Before running the power method, checks that matrix is square with non-negative
values, in accordance with the Perron-Frobenius theorem.
}
\seealso{
\code{\link[=rankings_from_eigenvalues]{rankings_from_eigenvalues()}}, \code{\link[=plot_lambdas]{plot_lambdas()}}
}
