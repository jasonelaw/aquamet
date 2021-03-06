% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nlaBankFeatures.r
\name{nlaBankFeatures}
\alias{nlaBankFeatures}
\title{Calculate NLA Aquatic Macrophyte Metrics}
\usage{
nlaBankFeatures(angle = NULL, drawdown = NULL, horizontalDistance = NULL,
  horizontalDistanceDrawdown = NULL, verticalHeight = NULL,
  verticalHeightDrawdown = NULL)
}
\arguments{
\item{angle}{A data frame containing bank angle class values for sites 
sampled using the boatable protocol, with the columns:
\itemize{
\item SITE an integer or character value identifying a single site 
visit.
\item STATION a character value identifying the station within the SITE
\item VALUE character string, with legal values of 'FLAT','GRADUAL','STEEP',
'NEAR_VERTICAL','NEAR_VERTICAL_UNDERCUT'.
}}

\item{drawdown}{A data frame containing drawdown value for sites sampled
using the boatable protocol, with the columns:
\itemize{
\item SITE an integer or character value identifying a single site visit.
\item STATION a character value identifying the station within the SITE
\item VALUE a character string indicating the presence of drawdown. Valid
values begin with Y for yes and N for No.
}}

\item{horizontalDistance}{A data frame containing the horizontal 
distance from waterline to high water mark (m) when flooding is present, 
with the columns:
\itemize{
\item SITE an integer or character value identifying a single site visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an numeric value, or character value that is castable to an 
numeric.
}}

\item{horizontalDistanceDrawdown}{A data frame containing the horizontal 
distance from waterline to high water mark (m) when drawdown is present, 
with the columns:
\itemize{
\item SITE an integer or character value identifying a single site visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an numeric value, or character value that is castable to an 
numeric.
}}

\item{verticalHeightDrawdown}{A data frame containing the vertical 
height from waterline to high water mark (m) when drawdown is present, 
with the columns:
\itemize{
\item SITE an integer or character value identifying a single site visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an numeric value, or character value that is castable to an 
numeric.
}}

\item{VerticalHeight}{A data frame containing the vertical 
height from waterline to high water mark (m) when flooding is present, 
with the columns:
\itemize{
\item SITE an integer or character value identifying a single site visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an numeric value, or character value that is castable to an 
numeric.
}}
}
\value{
Either a data frame when metric calculation is successful or a 
character string containing an error message when metric calculation 
is not successful. The data frame contains the following columns:
\itemize{ 
    \item SITE - unique site visit identifier
    \item METRIC - metric name
    \item VALUE - metric value
      }
The output metrics include:
BFOANGLE, BFFFLAT, BFFGRADUAL, BFFSTEEP, BFFVERTICAL, BFNANGLE, BFXHORIZDIST,
BFXHORIZDIST_DD, BFXVERTHEIGHT_DD, BFNHORIZDIST, BFNHORIZDIST_DD, 
BFNVERTHEIGHT_DD

Descriptions for all metrics are included in 
\emph{NLA_Physical_Habitat_Metric_Descriptions.pdf} in the package
documentation.
}
\description{
This function calculates the bank features portion of the physical
habitat metrics for National Lakes Assessment (NLA) data.
}
\examples{
  head(nlaPhabEx)
     
  exBankFeatures <- nlaBankFeatures(angle=subset(nlaPhabEx,PARAMETER=='ANGLE',select=-PARAMETER),
  drawdown=subset(nlaPhabEx,PARAMETER=='DRAWDOWN',select=-PARAMETER),
  horizontalDistance=subset(nlaPhabEx,PARAMETER=='HORIZ_DIST',select=-PARAMETER),
  horizontalDistanceDrawdown=subset(nlaPhabEx,PARAMETER='HORIZ_DIST_DD',select=-PARAMETER),
  verticalHeight=subset(nlaPhabEx,PARAMETER=='VERT_DIST',select=-PARAMETER),
  verticalHeightDrawdown=subset(nlaPhabEx,PARAMETER=='VERT_DIST_DD',select=-PARAMETER))
  
  head(exBankFeatures)
 
}
\author{
Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\keyword{survey}
