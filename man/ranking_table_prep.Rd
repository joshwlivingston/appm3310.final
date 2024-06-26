% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ranking_table_prep.R
\name{ranking_table_prep_direct_win_loss}
\alias{ranking_table_prep_direct_win_loss}
\alias{ranking_table_prep_points_scored_mean}
\alias{ranking_table_prep_points_proportion}
\alias{ranking_table_prep_points_squared_distance}
\alias{ranking_table_prep_points_abs_distance}
\alias{ranking_table_prep_points_sqrt_points}
\alias{ranking_table_prep_points_polynomial1}
\alias{ranking_table_prep_points_polynomial2}
\alias{ranking_table_prep_points_polynomial3}
\alias{ranking_table_prep_points_polynomial4}
\alias{ranking_table_prep_points_inverse}
\alias{ranking_table_prep_points_inverse2}
\alias{ranking_table_prep_points_inverse3}
\alias{ranking_table_prep_points_polynomial5}
\alias{ranking_table_prep_points_difference_cond}
\alias{ranking_table_prep_point_run_up}
\title{Ranking table preparation}
\usage{
ranking_table_prep_direct_win_loss(ncaa_data)

ranking_table_prep_points_scored_mean(ncaa_data)

ranking_table_prep_points_proportion(ncaa_data)

ranking_table_prep_points_squared_distance(ncaa_data)

ranking_table_prep_points_abs_distance(ncaa_data)

ranking_table_prep_points_sqrt_points(ncaa_data)

ranking_table_prep_points_polynomial1(ncaa_data)

ranking_table_prep_points_polynomial2(ncaa_data)

ranking_table_prep_points_polynomial3(ncaa_data)

ranking_table_prep_points_polynomial4(ncaa_data)

ranking_table_prep_points_inverse(ncaa_data)

ranking_table_prep_points_inverse2(ncaa_data)

ranking_table_prep_points_inverse3(ncaa_data)

ranking_table_prep_points_polynomial5(ncaa_data)

ranking_table_prep_points_difference_cond(ncaa_data)

ranking_table_prep_point_run_up(ncaa_data)
}
\arguments{
\item{ncaa_data}{Cleaned NCAA data returned from \code{\link[=clean_ncaa_data]{clean_ncaa_data()}}}
}
\value{
A ranking table ready to be passed to \code{\link[=to_matrix]{to_matrix()}}
}
\description{
Given cleaned NCAA data, compute a ranking table using various methods
}
\details{
\code{ranking_table_direct_win_loss()} assigns a 1 for each victory, a 0.5 for
each tie, and a 0 for loss, then adds the results.

\code{ranking_table_prep_points_scored_mean()} takes the average points scored across all
games between the two teams.

\code{ranking_table_prep_points_proportion()} takes the
points scored as a percentage of total points in that game.

\code{ranking_table_prep_points_squared_distance()} takes the difference between the
team's points and the opponent's points and squares the result.

\code{ranking_table_prep_points_abs_distance()} takes the difference between the
team's points and the opponent's points and takes the absolute value of the result
}
