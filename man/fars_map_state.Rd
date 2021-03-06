% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Draw a map of accident locations}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{State number as a character string or numeric value.}

\item{year}{Year as a character string or numeric value.}
}
\value{
State map with accidents locations.
}
\description{
This function crates a state map with US traffic accident locations
at selected year.
}
\note{
Maps can be produced only for one state and year at the time.
Years from 2013 to 2015 are available, function throws an error if
year(s) outside 2013-2015 or invalid state number is selected.
}
\examples{
\dontrun{
fars_map_state(1, 2014)
}
}
\references{
US National Highway Traffic Safety Administration
}
\seealso{
\code{\link{fars_summarize_years}} for summaries.
}
