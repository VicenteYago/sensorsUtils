% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicUtils.R
\name{make.aggregation}
\alias{make.aggregation}
\title{Aggregate dataframe with a function.}
\usage{
make.aggregation(df, freq, fun)
}
\arguments{
\item{df}{a dataframe, the first column must be the time index.}

\item{freq}{frequency of the observations in the dataframe, same as \code{\link[stats]{ts}} object.}

\item{fun}{a function like \code{mean()}, \code{max()}, etc.}
}
\description{
Performs an aggregation within the interval defined by \code{freq}, by executing
a provided function.
}
\details{
For security reasons the dataframe must have a number of observations multiple
of \code{freq}, also all the time index must be complete i.e without silents losts, see
\code{\link[sensorsUtils]{fortify_df}} for achieve this.

Its desirable that the time interval present in the input dataframe include complete periods of time. For example
if the time index (first column), takes the range ['2019/01/01 00:00:00', ..., '2019/01/03 20:00:00'] will be more
convenient to modify it to ['2019/01/01 00:00:00', ..., '2019/01/03 23:00:00'] for obtain the last day completely,
that effect can be acomplish with the function \code{\link[sensorsUtils]{fortify_df}}, using the parameter \code{fInterval}.
}
\examples{
# Mean of 3 day data by days
a = as.POSIXct("2019/01/01", tz = "UTC")
b = as.POSIXct("2019/01/03 23:00:00", tz = "UTC")
df <- data.frame(dates = seq.POSIXt(a,b,"hour"), A = runif(24*3))
make.aggregation(df, freq = 24, mean)

# The same with multicolumn datadrame
a = as.POSIXct("2019/01/01", tz = "UTC")
b = as.POSIXct("2019/01/03 23:00:00", tz = "UTC")
df<-data.frame(dates = seq.POSIXt(a,b,"hour"),
              A = runif(24*3),
              B = rep(1:3,each=24))
make.aggregation(df, freq = 24, mean)

# The previus example but using a 12 hour mean
a = as.POSIXct("2019/01/01", tz = "UTC")
b = as.POSIXct("2019/01/03 23:00:00", tz = "UTC")
df<-data.frame(dates = seq.POSIXt(a,b,"hour"),
              A = runif(24*3),
              B = rep(1:3,each=24))
make.aggregation(df, freq = 12, mean)
}
\author{
JV Yago
}
