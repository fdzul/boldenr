% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_state_serotype.R
\name{plot_state_serotype}
\alias{plot_state_serotype}
\title{Plot the dengue cases by state and serotype}
\usage{
plot_state_serotype(dataset, year, scale_serotype, x_serotype, y_serotype)
}
\arguments{
\item{dataset}{is the dataset of \href{**SINAVE**}{http://vectores.sinave.gob.mx/}.}

\item{year}{is the year of dataset.}

\item{scale_serotype}{is the scale of the dengue serotype plot.}

\item{x_serotype}{The x location of the dengue serotype plot. Sea also cowplot::draw_plot?.}

\item{y_serotype}{The y location of the dengue serotype plot. Sea also cowplot::draw_plot?.}
}
\value{
a plot of class ggplot.
}
\description{
Plot the dengue cases by state and serotype
}
\examples{
1+1
}
\seealso{
\link[cowplot]{draw_plot} and \link[cowplot]{ggdraw}.
}
\author{
Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
}
