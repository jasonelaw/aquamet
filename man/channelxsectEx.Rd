\name{channelxsectEx}
\alias{channelxsectEx}
\docType{data}
\title{Example Channel Cross-section Dataset}
\description{
  A dataset containing raw channel cross-section physical habitat data for use in function examples.
}
\usage{data(channelxsectEx)}
\format{
  A data frame with 560 observations on the following 7 variables.
  \describe{
    \item{SITE}{unique site visit ID.}
    \item{TRANSECT}{reach transect label.}
    \item{BANK}{Bank location: RT=Right,LF=Left,NONE=not applicable.}
    \item{PARAMETER}{character variable of parameter measured in field.}
    \item{VALUE}{value of measured parameter.}
    \item{FLAG}{flag of value of measured parameter.}
    \item{SAMPLE_TYPE}{indicator of field form from which data obtained.}
  }
}
\details{
  These data are a small subset of the NRSA 2008-2009 channel cross-section dataset for example purposes only.
}
\examples{
data(channelxsectEx)
head(channelxsectEx)
}
\keyword{datasets}
