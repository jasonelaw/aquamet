% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nlaBottomSubstrate.r
\name{nlaBottomSubstrate}
\alias{nlaBottomSubstrate}
\title{Calculate NLA Bottom Substrate Metrics}
\usage{
nlaBottomSubstrate(bedrock = NULL, boulder = NULL, color = NULL,
  cobble = NULL, gravel = NULL, odor = NULL, organic = NULL,
  sand = NULL, silt = NULL, wood = NULL,
  substrateCovers = data.frame(VALUE = c(NA, "0", "1", "2", "3", "4"), cover =
  c(NA, 0, 0.05, 0.25, 0.575, 0.875), presence = as.integer(c(NA, 0, 1, 1, 1,
  1)), stringsAsFactors = FALSE), substrateSizes = data.frame(CLASS =
  c("BEDROCK", "BOULDERS", "COBBLE", "GRAVEL", "SAND", "SILT", "ORGANIC",
  "WOOD"), diam = c(gmean(c(4000, 8000)), gmean(c(250, 4000)), gmean(c(64,
  250)), gmean(c(2, 64)), gmean(c(0.06, 2)), gmean(c(0.001, 0.06)), NA, NA),
  inPopulationEstimate = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  stringsAsFactors = FALSE))
}
\arguments{
\item{bedrock}{A data frame containing bedrock class values, 
with the columns:
\itemize{
\item SITE an integer or character value identifying a single site 
visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an integer value, or character value that is castable to an 
integer, from 0-4 containing the bedrock bottom substrate cover
category.
}}

\item{boulder}{A data frame containing boulder class values, 
with the columns:
\itemize{
\item SITE an integer or character value identifying a single site 
visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an integer value, or character value that is castable to an 
integer, from 0-4 containing the boulder bottom substrate cover
category.
}}

\item{color}{A data frame containing bottom substrate color values, 
with the columns:
\itemize{
\item SITE an integer or character value identifying a single site 
visit.
\item STATION a character value identifying the station within the SITE
\item VALUE a character string with possible values of BLACK, GRAY,
BROWN, RED, or OTHER_XXX representing the bottom substrate color category.
}}

\item{cobble}{A data frame containing cobble bottom substrate class 
values, with the columns:
\itemize{
\item SITE an integer or character value identifying a single site 
visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an integer value, or character value that is castable to an 
integer, from 0-4 containing the cobble bottom substrate cover
category.
}}

\item{gravel}{A data frame containing gravel bottom substrate class 
values, with the columns:
\itemize{
\item SITE an integer or character value identifying a single site 
visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an integer value, or character value that is castable to an 
integer, from 0-4 containing the gravel bottom substrate cover
category.
}}

\item{odor}{A data frame containing bottom substrate odor class 
values, with the columns:
\itemize{
\item SITE an integer or character value identifying a single site 
visit.
\item STATION a character value identifying the station within the SITE
\item VALUE a character string with possible values of NONE, CHEMICAL,
OIL, ANOXIC, H2S, or OTHER_XXX representing the bottom substrate odor category.
}}

\item{organic}{A data frame containing organic bottom substrate class 
values, with the columns:
\itemize{
\item SITE an integer or character value identifying a single site 
visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an integer value, or character value that is castable to an 
integer, from 0-4 containing the organic bottom substrate cover
category.
}}

\item{sand}{A data frame containing sand bottom substrate class 
values, with the columns:
\itemize{
\item SITE an integer or character value identifying a single site 
visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an integer value, or character value that is castable to an 
integer, from 0-4 containing the sand bottom substrate cover
category.
}}

\item{silt}{A data frame containing silt bottom substrate class 
values, with the columns:
\itemize{
\item SITE an integer or character value identifying a single site 
visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an integer value, or character value that is castable to an 
integer, from 0-4 containing the silt bottom substrate cover
category.
}}

\item{wood}{A data frame containing wood bottom substrate class 
values, with the columns:
\itemize{
\item SITE an integer or character value identifying a single site 
visit.
\item STATION a character value identifying the station within the SITE
\item VALUE an integer value, or character value that is castable to an 
integer, from 0-4 containing the wood bottom substrate cover
category.
}}

\item{substrateCovers}{a data frame containing substrate cover category 
values (VALUE), the lower proportional cover value for each category (cover),
and an indicator variable of presence of a substrate type for each category.
The default values are:
\itemize{
\item VALUE '0', '1', '2', '3', '4', NA
\item cover 0, 0.05, 0.25, 0.575, 0.875, NA
\item presence 0, 1, 1, 1, 1, NA
}}

\item{substrateSizes}{a data frame containing substrate class names (CLASS)
and corresponding geometric mean of diameter ranges in mm (diam), as well as
an indicator of whether to include the class in estimates of substrate size
for the site. 
The default values are: 
\itemize{
\item CLASS 'BEDROCK', 'BOULDERS','COBBLE', 'GRAVEL', 'SAND', 'SILT', 
'ORGANIC', 'WOOD'
\item diam 5656.854, 1000, 126.4911, 11.31371, 0.3464102, 0.007745967, 
NA, NA  
\item inPopulationEstimate TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE
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
BSFPBEDROCK, BSFPBOULDERS, BSFPCOBBLE, BSFPGRAVEL, BSFPORGANIC, BSFPSAND,
BSFPSILT, BSFPWOOD, BSISTAVARIETY, BSISITEVARIETY, BSFCBEDROCK, BSFCBOULDERS,
BSFCCOBBLE, BSFCGRAVEL, BSFCORGANIC, BSFCSAND, BSFCSILT, BSFCWOOD, 
BSVBEDROCK, BSVBOULDERS, BSVCOBBLE, BSVGRAVEL, BSVORGANIC, BSVSAND,      
BSVSILT, BSVWOOD, BSNBEDROCK, BSNBOULDERS, BSNCOBBLE, BSNGRAVEL, BSNORGANIC, 
BSNSAND, BSNSILT, BSNWOOD, BSXLDIA, BSVLDIA, BS16LDIA, BS25LDIA, BS50LDIA, 
BS75LDIA, BS84LDIA, BSOPCLASS, BSOFCLASS, BSFBLACK, BSFBROWN, BSFGRAY, BSFRED, 
BSFOTHERCOLOR, BSNCOLOR, BSOCOLOR, BSFANOXIC, BSFCHEMICAL, BSFH2S, BSFNONEODOR,
BSFOTHERODOR, BSFOIL, BSNODOR, BSOODOR 

Descriptions for all metrics are included in 
\emph{NLA_Physical_Habitat_Metric_Descriptions.pdf} in the package
documentation.
}
\description{
This function calculates the bottom substrate portion of the physical
habitat metrics for National Lakes Assessment (NLA) data.
}
\examples{
  head(nlaPhabEx)
  
  bedrock <- subset(nlaPhabEx,PARAMETER=='BS_BEDROCK',select=-PARAMETER)
  boulder <- subset(nlaPhabEx,PARAMETER=='BS_BOULDER',select=-PARAMETER)
  color <- subset(nlaPhabEx,PARAMETER=='BS_COLOR',select=-PARAMETER)
  cobble <- subset(nlaPhabEx,PARAMETER=='BS_COBBLE',select=-PARAMETER)
  gravel <- subset(nlaPhabEx,PARAMETER=='BS_GRAVEL',select=-PARAMETER)
  odor <- subset(nlaPhabEx,PARAMETER=='ODOR',select=-PARAMETER)
  organic <- subset(nlaPhabEx,PARAMETER=='BS_ORGANIC',select=-PARAMETER)
  sand <- subset(nlaPhabEx,PARAMETER=='BS_SAND',select=-PARAMETER)
  silt <- subset(nlaPhabEx,PARAMETER=='BS_SILT',select=-PARAMETER)
  wood <- subset(nlaPhabEx,PARAMETER=='BS_WOOD',select=-PARAMETER)
  
  exBottomSubstrate <- nlaBottomSubstrate(bedrock,boulder,color,
  cobble,gravel,odor,organic,sand,silt,wood)
  
  head(exBottomSubstrate)
 
}
\author{
Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\keyword{survey}
