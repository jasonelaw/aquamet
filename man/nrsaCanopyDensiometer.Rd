% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nrsaCanopyDensiometer.r
\name{nrsaCanopyDensiometer}
\alias{nrsaCanopyDensiometer}
\title{Calculate NRSA Canopy Densiometer Metrics}
\usage{
nrsaCanopyDensiometer(bDensiom = NULL, wDensiom = NULL,
  wChannelBank = c("LF", "RT"), wChannelMid = c("CU", "CL", "CD", "CR"))
}
\arguments{
\item{bDensiom}{A data frame containing canopy densiometer 
counts for boatable reaches, with the following columns:
\itemize{
\item SITE integer or character specifying the site visit
\item VALUE numeric counts
}}

\item{wDensiom}{A data frame containing canopy densiometer 
counts for wadeable reaches, with the following columns:
\itemize{
\item SITE integer or character specifying the site visit
\item DIRECTION character value specifying the direction being
 faced.  These values are expected to be the same
 as listed in the arguments wChannelBank and wChannelMid
\item VALUE numeric counts
}}

\item{wChannelBank}{Character values listing the values of 
DIRECTION which indicate that the counts were obtained on the 
bank of the channel. Defaults to c('LF','RT').}

\item{wChannelMid}{Character values listing the values of DIRECTION 
which indicate that the counts were obtained midstream in the channel. 
Defaults to c('CU','CL','CD','CR').}
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
The output metrics for boatable sites include: xcdenmid, 
vcdenmid, nmid, xcdenbk, vcdenbk, nbnk 
 
Descriptions for all metrics are included in 
\emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
documentation.
}
\description{
This function calculates the canopy densiometer 
portion of the physical habitat metrics for National Rivers 
and Streams Assessment (NRSA) data.  The function requires a 
data frame containing the channel cover data file.
}
\examples{
head(channelcoverEx)
channelcoverEx <- plyr::mutate(channelcoverEx, VALUE=as.numeric(VALUE))

bDen <- subset(channelcoverEx,SAMPLE_TYPE=='PHAB_CHANB' & PARAMETER=='DENSIOM')

wDen <- subset(channelcoverEx,SAMPLE_TYPE=='PHAB_CHANW' & PARAMETER=='DENSIOM')
wDen <- plyr::rename(wDen,c('BANK'='DIRECTION'))

canDenOut <- nrsaCanopyDensiometer(bDensiom=bDen, wDensiom=wDen)
head(canDenOut)
}
\author{
Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
