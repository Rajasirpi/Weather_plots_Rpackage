% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d2m.R
\name{d2m}
\alias{d2m}
\title{Daily to monthly rainfall conversion}
\usage{
d2m(data, date, rainfall)
}
\arguments{
\item{data}{A file path of the csv file as a string within double quotes. For example use Prec_data.csv demo file using system.file()}

\item{date}{Column name which contain date and year as a string within double quotes. Date has to be in year-month-day format.}

\item{rainfall}{column name which contain rainfall/precipitation values as string within double quotes.}
}
\value{
a data frame
}
\description{
The function "d2m" can be used to Daily rainfall values to monthly values.
}
\examples{
file= system.file("extdata","Prec_data.csv", package = "Rwplots")
d2m(file,"time","Rainfall")
}
