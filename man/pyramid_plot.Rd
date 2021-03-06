% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pyramid_plot.R
\name{pyramid_plot}
\alias{pyramid_plot}
\title{Generate a pyramid plot by health jurisdiction or state.}
\usage{
pyramid_plot(x, by_juris, state, year, pal)
}
\arguments{
\item{x}{is the epidemiological dataset.}

\item{by_juris}{is the parameter for define the pyramid plot. if by_juris is false the plot is the state, else the plot is by jurisdiction.}

\item{state}{is the parameter for select the state.}

\item{year}{is the current year.}

\item{pal}{is the color of palette.}
}
\value{
a pyramid plot of class ggplot.
}
\description{
Generate a pyramid plot by health jurisdiction or state.
}
\details{
xxx
}
\examples{
1+1

}
\references{
xxxxx
}
\seealso{
\link[boldenr]{epidemiological_channel}
}
\author{
Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
}
