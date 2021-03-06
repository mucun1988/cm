% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/routine.R
\name{find_infty_step_map}
\alias{find_infty_step_map}
\title{find the final retired id and active id after infinity step}
\usage{
find_infty_step_map(map_one_step)
}
\description{
find the final retired id and active id after infinity step
}
\examples{
one_step_map <- data.table(retired_id = c('A','B','C', 'E'), active_id = c('B','C','D','F'))
map_infty_step <- find_infty_step_map(one_step_map)
}
